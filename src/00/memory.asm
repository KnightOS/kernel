;; formatMem [System]
;;  Formats memory in preparation for memory allocation.
;; Notes:
;;  This function will deallocate **all allocated memory**.
formatMem:
    ld a, 0xFF
    ld (userMemory), a
    ld hl, kernelMem - (userMemory - kernelMem) - 5 ; Total RAM - Kernel RAM Size - Formatting Overhead + 1
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
        push bc \ pop de
        call DEMulA
        push hl \ pop bc
        call malloc
        jr nz, .fail
        xor a
        call memset
.fail:
    pop hl
    pop de
    pop bc
    pop af
    ret
    
; Sets the entire allocated section to A
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

; Inputs:    IX is somewhere within pre-allocated memory
; Outputs:    IX points to the start of that memory
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
    
; Input: IX is somewhere in pre-allocated memory
; Outputs: IX is the last byte of that memory
memSeekToEnd:
    call memSeekToStart
    push hl
    push bc
        push ix \ pop hl
        dec hl \ ld b, (hl)
        dec hl \ ld c, (hl)
        inc hl \ inc hl \ add hl, bc
        push hl \ pop ix
    pop bc
    pop hl
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
    push af
    ld a, i
    push af
    di
    push hl
    push de
    push bc
        ld hl, userMemory
.searchLoop:
        ld a, (hl)
        inc hl
        ld e, (hl)
        inc hl
        ld d, (hl)
        inc hl ; Load owner thread into A, size into DE, HL points to first byte of allocated section
        
        cp nullThread ; Free memory is owned by the null thread ($FF)
        jr z, .handleFree
        inc de
        inc de
.insufficientFree:
        add hl, de ; Skip non-free section
        jp c, .outOfMem ; If overflow
        
        jr .searchLoop
        
.handleFree:
        ; BC = amount to allocate
        ; DE = size of current section
        ; HL = pointer to section start
        ; The overhead of splitting a section is 5 bytes, for the new header/footer combo
        ; If the amount to allocate is within 5 bytes of the avaiable space, it is padded to
        ; fill the entire space.
        
        ex de, hl
        or a
        push hl
        sbc hl, bc
        pop hl
        ex de, hl
        jr nc, _
        ; Return to loop
        add hl, de
        inc hl \ inc hl
        jr .searchLoop
        
_:      ; Check for dead pockets
        push de
            ex de, hl
            or a
            sbc hl, bc
            xor a
            cp h
            jr nz, _
            ld a, 5
            cp l
            jr c, _
            ; Fill up pocket
            push bc \ push hl \ pop bc \ pop hl ; ex hl, bc
            add hl, bc
            push hl \ pop bc ; ld bc, hl
_:          ex de, hl
        pop de
        
        call cpBCDE
        jr z, .skipNewMeta
        
.doAllocNormal: ; Not accounting for dead pockets
    ; Update existing metadata (allocated header)
    push de
        push hl
            push hl \ pop ix ; Set IX for the return value
            dec hl
            ld (hl), b
            dec hl
            ld (hl), c
            dec hl
            call getCurrentThreadID
            ld (hl), a
            push hl \ pop de
        pop hl
        
        add hl, bc ; HL points to footer of new section
        ld (hl), e
        inc hl
        ld (hl), d ; Add footer
    pop de
    
    inc hl
    ld (hl), nullThread ; Add header (thread id)
    inc hl
    
    ex de, hl
    or a
    sbc hl, bc
    dec hl \ dec hl \ dec hl \ dec hl \ dec hl ; Account for meta overhead
    ex de, hl
    push hl ; Save location of header
        ld (hl), e ; Add header (size)
        inc hl
        ld (hl), d
        inc hl
    
        ; Update existing metadata (old header)
        add hl, de
    pop de
    dec de
    ld (hl), e
    inc hl
    ld (hl), d
    
    pop bc
    pop de
    pop hl
    
    pop af
    jp po, _
    ei
_:  pop af
    cp a
    ret
        
.skipNewMeta:
    ; Update existing metadata (allocated header)
    push hl \ pop ix ; Set IX for the return value
    dec hl \ dec hl \ dec hl
    call getCurrentThreadID
    ld (hl), a
    
    pop bc
    pop de
    pop hl
    
    pop af
    jp po, _
    ei
_:  pop af
    cp a
    ret
        
.outOfMem:
    pop bc
    pop de
    pop hl
    
    pop af
    jp po, _
    ei
_:  pop af

    cp 1 ; Set NZ for failure
    ld a, errOutOfMem
    ret
    
; Inputs:    IX: Pointer to first byte of previously allocated memory
; Outputs:    None
free:
    push af
    ld a, i
    push af
    di
    push bc
    push hl
    push de
    push ix
        push ix \ pop hl  ; LD HL, IX
        ; Unallocate the referenced block
        dec hl
        ld b, (hl)
        dec hl
        ld c, (hl) ; Size of the freed section
        dec hl
        ld (hl), nullThread
        
        ; Attempt a backward merge
        dec hl
        ld d, (hl)
        dec hl
        ld e, (hl)
        ld h, d
        ld l, e ; LD HL, DE
        
        ld a, (hl)
        cp nullThread
        jr nz, .tryMergeForward
        
        ; Possible to merge backward
        inc bc \ inc bc \ inc bc \ inc bc \ inc bc ; BC += 5
        inc hl ; Update location pointer to size of leading block
        ld e, (hl)
        inc hl
        ld d, (hl)
        ex de, hl
        ; DE is pointer, HL is size
        add hl, bc ; Change size to leading+freed+5 [overhead of 5 comes from the merging of the two headers and footers]
        ex de, hl
        ; HL is pointer, DE is new size
        ld (hl), d
        dec hl
        ld (hl), e
        dec hl
        ld b, d
        ld c, e ; New size of merged block in BC [allows for forward merging]
        ld d, h
        ld e, l ; New header at (de)
        inc hl \ inc hl \ inc hl
        add hl, bc ; Pointing to new footer (needs to be updated)
        ld (hl), e
        inc hl
        ld (hl), d ; Update footer
        ld h, d
        ld l, e ; Prepare for forward merge
        inc hl \ inc hl \ inc hl
        push hl
        pop ix
        jr _ ; Skip part of the forward merge code
.tryMergeForward:
        push ix \ pop hl
_:
        ; HL is the first byte of this block, BC is the size
        add hl, bc ; HL points to the first byte of the trailing footer
        inc hl \ inc hl
        ld a, (hl)
        cp nullThread
        inc hl
        ld e, (hl)
        inc hl
        ld d, (hl) ; DE == size of leading block
        jr nz, .done
        
        ; Merge forward
        ;inc hl?
        add hl, de ; HL points to footer of leading block
        ex de, hl
        ; DE points to footer of leading block
        ; HL is size of leading block
        inc hl \ inc hl \ inc hl \ inc hl \ inc hl
        add hl, bc
        ex de, hl ; DE now has combined size of two blocks
        push de
            push ix \ pop de
            dec de \ dec de \ dec de
            ld (hl), e
            inc hl
            ld (hl), d
        pop de
        push ix \ pop hl
        dec hl
        ld (hl), d
        dec hl
        ld (hl), e ; Update header
        
.done:
    pop ix
    pop de
    pop hl
    pop bc
    
    pop af
    jp po, _
    ei
_:  pop af
    ret
    