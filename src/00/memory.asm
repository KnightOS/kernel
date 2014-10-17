; formatMem [System]
;  Formats memory in preparation for memory allocation.
; Notes:
;  This function will deallocate **all allocated memory**.
formatMem:
    ld a, 0xFF
    ld (userMemory), a
    ld hl, (0x10000 - userMemory) - 5 ; Total RAM - Kernel RAM Size - Formatting Overhead + 1
    ld (userMemory + 1), hl
    ld hl, userMemory
    ld (0xFFFE), hl
    ret

;; allocScreenBuffer [Display]
;;  Allocates a 768-byte screen buffer.
;; Outputs:
;;  IY: Screen buffer
allocScreenBuffer:
    push bc
    push ix
        ld bc, 768
        call malloc
        push ix \ pop iy
    pop ix
    pop bc
    ret

;; freeScreenBuffer [Display]
;;  Deallocates a screen buffer allocated with [[allocScreenBuffer]]
;; Inputs:
;;  IY: Screen buffer
freeScreenBuffer:
    push ix
        push iy \ pop ix
        call free
    pop ix
    ret

;; reassignMemory [System]
;;  Reassigns a given block of memory to the specified thread ID.
;; Inputs:
;;  IX: Pointer to any location within the target block.
;;  A: Thread ID for new owner
reassignMemory:
    push ix
        ; TODO: Check if thread exists
        call memSeekToStart
        ld (ix + -3), a
    pop ix
    ret

;; calloc [System]
;;  Allocates memory for a given number of elements
;;  of a given size (that is, BC * A bytes total),
;;  then fills it with zeros.
;; Inputs:
;;  BC: Number of elements
;;  A: Size of element
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  IX: First byte of allocated and zeroed memory (on success)
calloc:
    push af
    push bc
    push de
    push hl
        push af \ push bc \ pop de
        call mul16By8
        push hl \ pop bc \ pop af
        call malloc
        jr nz, .fail
        xor a
        call memset
    pop hl
    pop de
    pop bc
    pop af
    cp a
    ret
.fail:
    pop hl
    pop de
    pop bc
    inc sp \ inc sp ;pop af
    ret
    
;; memset [System]
;;  Sets the value of an entire allocated section of memory.
;; Inputs:
;;  A: Value to set
;;  IX: Pointer to anywhere in allocated section
memset:
    push ix
    push bc
    push hl
    push de
        call memSeekToStart
        push ix \ pop hl \ push ix \ pop de
        inc de
        ld (hl), a
        ld c, (IX + -2) \ ld b, (IX + -1)
        dec bc
        ldir
    pop de
    pop hl
    pop bc
    pop ix
    ret

;; memSeekToStart [System]
;;  Move IX to the beginning of the memory section it points to.
;; Inputs:
;;  IX: Pointer to anywhere in a section of allocated memory
;; Outputs:
;;  IX: Pointer to first byte of section
memSeekToStart:
    push hl
    push bc
    push de
        push ix \ pop de
        ld hl, userMemory
.loop:
        inc hl
        ld c, (hl)
        inc hl
        ld b, (hl)
        inc hl
        add hl, bc
        jr c, _
        call cpHLDE
        jr nc, ++_
        inc hl \ inc hl
        jr .loop
_:      ld ix, 0 ; Error
        jr ++_
_:      sbc hl, bc
        push hl \ pop ix
_:  pop de
    pop bc
    pop hl
    ret
    
;; memSeekToEnd [System]
;;  Move IX to the end of the memory section it points to.
;; Inputs:
;;  IX: Pointer to anywhere in a section of allocated memory
;; Outputs:
;;  IX: Pointer to last byte of section
memSeekToEnd:
    call memSeekToStart
    push hl
    push bc
        push ix \ pop hl
        dec hl \ ld b, (hl)
        dec hl \ ld c, (hl)
        inc hl \ add hl, bc
        push hl \ pop ix
    pop bc
    pop hl
    ret

;; memcheck [System]
;;  Walks over memory and makes sure nothing has corrupted the allocation list.
;; Outputs:
;;  Z: Set if OK, reset if broken
;; Notes:
;;  A reboot is probably required if this returns NZ.
memcheck:
    push af
    ld a, i
    push af
    di
    push hl
    push bc
    push de
        ld hl, userMemory
.loop:
        push hl
            inc hl \ ld c, (hl) ; Size of this block into BC
            inc hl \ ld b, (hl)
            inc hl
            add hl, bc ; Move HL to block footer
        pop bc
        ld e, (hl) \ inc hl
        ld d, (hl) \ inc hl
        call cpBCDE
        jr nz, .fail
        ld a, h
        cp 0
        jr nz, _
        ld a, l
        cp 0
        jr z, .done
_:      cp 0x80
        jr c, .fail
        jr .loop
.done:
    pop de
    pop bc
    pop hl
    pop af
    jp po, _
    ei
_:  pop af
    cp a
    ret
.fail:
    pop de
    pop bc
    pop hl
    pop af
    jp po, _
    ei
_:  pop af
    or 1
    ret

;; malloc [System]
;;  Allocates the specified amount of memory.
;; Inputs:
;;  BC: Length of requested section, in bytes
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  IX: First byte of allocated memory (on success)
malloc:
    ; KnightOS uses a simple linked list for memory allocation. Memory starts of as one complete
    ; block of memory allocated to thread 0xFF (i.e. free), and each allocation divides this into
    ; more and more blocks. A block looks something like this:
    ;
    ; OO SSSS .... AAAA
    ; Where OO is the owner of this block, SSSS is the size of the block, and AAAA is the address
    ; of the header (starting at OO).
    push af
    ld a, i
    push af
    di
    push hl
    push de
    push bc
        ld d, b \ ld e, c ; Save desired size in DE
        ld hl, userMemory
.identify_loop: ; Identify a block to use
        ld a, (hl) \ inc hl
        ld c, (hl) \ inc hl
        ld b, (hl) \ inc hl
        ;   A: Block owner
        ;  BC: Block length
        ; *HL: First byte of block
        ; ..
        ; Check if free
        cp 0xFF
        jr nz, .continue_loop
        ; Check if acceptable size
        call cpBCDE
        jr z, .allocate_this ; If exactly the right size
        jr c, .continue_loop ; If too small
.allocate_this:
        dec hl \ dec hl \ dec hl
        jr .do_allocate
.continue_loop:
        ; Continue to next block
        add hl, bc
        inc hl \ inc hl
        ; Check to see if HL rolled over
        xor a \ cp h
        jr nz, .identify_loop
        jp .out_of_memory
.do_allocate:
        ; *HL: Header of block to use
        ;  DE: Size of block to allocate
        call getCurrentThreadID
        ld (hl), a \ inc hl ; Set new owner
        ; Get the old size and write the new size
        ld c, (hl) \ ld (hl), e \ inc hl
        ld b, (hl) \ ld (hl), d \ inc hl
        call .pad_if_needed
        push hl \ pop ix ; IX to malloc return value
        push hl
        push de
            push de \ push bc \ pop hl \ pop bc
            or a ; Reset carry
            sbc hl, bc
            ld bc, 5 ; Overhead
            sbc hl, bc
            jr z, .exact_fit
            ld b, h \ ld c, l
            ; BC: Size of new free block
        pop de
        pop hl
        add hl, de ; HL to footer of new block
        push ix \ pop de
        dec de \ dec de \ dec de ; DE to header
        ld (hl), e \ inc hl
        ld (hl), d \ inc hl ; Fill in pointer to previous block
        push hl
            ld a, 0xFF
            ld (hl), a \ inc hl ; Mark next one as free
            ld (hl), c \ inc hl
            ld (hl), b \ inc hl ; Size of block
            add hl, bc ; Move to footer
        pop de
        ld (hl), e \ inc hl
        ld (hl), d ; Pointer to footer
.finish:
    pop bc
    pop de
    pop hl
    pop af
    jp po, _
    ei
_:  pop af
    cp a
    ret
.exact_fit:
        ; No need to write a new free header, just set the footer for the new block
        pop de
        pop hl
        add hl, de
        push ix \ pop de
        ld (hl), e \ inc hl
        ld (hl), d
        jr .finish
.pad_if_needed:
        ; TODO: Subtract BC from DE and see if the result is <= 5.
        ; If so, add the remainder to the original DE and write it back to the block header
        ; Which is at HL-3
        ; Must leave all relevant registers intact
        ret
.out_of_memory:
    pop bc
    pop de
    pop hl
    pop af
    jp po, _
    ei
_:  pop af
    or 1
    ld a, errOutOfMem
    ret
    
;; free [System]
;;  Frees a previously allocated section of memory
;; Inputs:
;;  IX: Pointer to first byte of section
free:
    ; Procedure:
    ;  1. Assign block to 0xFF
    ;  2. Check if next block is free - if so, merge
    ;  3. Check if previous block is free - if so, merge
    push af
    ld a, i
    push af
    di
    push bc
    push hl
    push de
    push ix
.free_block:
        ld a, 0xFF
        ld (ix + -3), a ; Free block
        ld c, (ix + -2)
        ld b, (ix + -1) ; Size of block in BC
.check_next_block:
    pop hl \ push hl ; IX into HL
        add hl, bc
        ; Note: We can check if HL == -2 here and if so, we can't merge forward (last block)
        inc hl \ inc hl ; HL to next block
        ld a, 0xFF
        cp (hl)
        jp z, .merge_forward_block
.check_previous_block:
    pop hl \ push hl ; IX into HL
        dec hl \ dec hl \ dec hl ; Move to header
        ld bc, userMemory
        call cpHLBC
        ; If this is the first block, we can't merge backwards, so don't bother
        jr z, .cannot_merge_previous
        dec hl \ ld d, (hl)
        dec hl \ ld e, (hl)
        ex de, hl
        ld a, 0xFF
        cp (hl)
        jr nz, .cannot_merge_previous
.merge_previous_block:
        ld c, (ix + -2)
        ld b, (ix + -1) ; Size of current block in BC
        inc hl \ ld e, (hl)
        inc hl \ ld d, (hl)
        ex de, hl
        add hl, bc
        ld bc, 5
        add hl, bc
        ex de, hl
        ; Size of combined blocks in DE
        ld (hl), d \ dec hl
        ld (hl), e \ dec hl ; Update header
        ; Skip to footer
        ld b, h \ ld c, l
        ex de, hl
        add hl, bc
        inc hl \ inc hl \ inc hl
        ld (hl), e \ inc hl ; Write header address here
        ld (hl), d
.cannot_merge_previous:
    pop ix
    pop de
    pop hl
    pop bc
    pop af
    jp po, _
    ei
_:  pop af
    ret
.merge_forward_block:
        ; Assumes IX is address of freed block
        ; Assumes HL points to forward block header
        ; Assumes BC is size of freed block
        inc hl \ ld e, (hl)
        inc hl \ ld d, (hl)
        ; DE is size of this block
        ex de, hl
        add hl, bc
        ld bc, 5
        add hl, bc
        ex de, hl
        ; DE is combined size, update freed block
        ld (ix + -1), d
        ld (ix + -2), e
    pop hl \ push hl
        add hl, de
        ; Update footer to point to new header
    pop de \ push de ; Grab IX
        dec de \ dec de \ dec de
        ld (hl), e \ inc hl
        ld (hl), d
        ; Forward merge complete!
        jr .check_previous_block
