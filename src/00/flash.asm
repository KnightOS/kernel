; TODO: Add routines to erase certificate sectors (or add this to eraseFlashSector?)
    rst 0 ; Prevent runaway code from unlocking flash

;; unlockFlash [Flash]
;;  Unlocks Flash and unlocks protected ports.
;; Notes:
;;  **Do not use this unless you know what you're doing.**
;;  
;;  Please call [[lockFlash]] when you finish what you're doing and don't spend too
;;  much time with Flash unlocked. Disable interrupts while Flash is unlocked.
unlockFlash:
    push af
    push bc
        getBankA
        push af
            setBankA(privledgedPage)
            ld b, 0x01
            ld c, 0x14
            call 0x4001
        pop af
        setBankA
    pop bc
    pop af
    ret

;; lockFlash [Flash]
;;  Locks Flash and locks protected ports.
lockFlash:
    push af
    push bc
        getBankA
        push af
            setBankA(privledgedPage)
            ld b, 0x00
            ld c, 0x14
            call 0x4004
        pop af
        setBankA
    pop bc
    pop af
    ret

;; writeFlashByte [Flash]
;;  Writes a single byte to Flash.
;; Inputs:
;;  HL: Destination
;;  A: Value
;; Notes:
;;  Flash must be unlocked. This can only *reset* bits of Flash.
writeFlashByte:
; The procedure is thus:
;    0xAA -> (0xAAA)
;    0x55 -> (0x555)
;    0xA0 -> (0xAAA)
;    DATA -> (PDEST)
;    Poll to completion
    push hl
    push bc
    push de
    push af
        ld b, a
        ld a, i
        push af
            di
            ld a, b
            and (hl) ; Remove any bits that cannot be written
            push hl
                ld hl, .ram
                ld de, flashFunctions
                ld bc, .ram_end - .ram
                ldir
            pop hl
            ld b, a
            jp flashFunctions
.return:
        pop af
        jp po, _
        ei
_:  pop af
    pop de
    pop bc
    pop hl
    ret
.ram:
    ld a, 0xF0
    ld (0), a ; Reset to be safe
    ld a, 0xAA
    ld (0xAAA), a
    ld a, 0x55
    ld (0x555), a
    ld a, 0xA0
    ld (0xAAA), a
    ld (hl), b
.poll:
    ; To poll, read the data back at (hl) to get the chip status. This is
    ; referred to as Q. The data programmed is B.
    ; 1. If Q7 == B7, we're done.
    ; 2. If Q5 == 1, poll again.
    ; 3. Read (HL) into Q again.
    ; 4. If Q7 == B7, we're done, otherwise the program failed.
    ld c, (hl)
    ld a, c
    xor b
    bit 7, a
    jp z, .return ; 1: Check Q7 == B7 and return if so
    bit 5, c      ; 2: Check Q5 != 0 and poll if so
    jr z, .poll
    ld a, (hl)    ; 3: Read again
    xor b
    bit 7, a
    jp z, .return ; 4: Check Q7 == B7 and return if so
    ; Operation failed, abort
    ld a, 0xF0
    ld (0), a
    jp .return
.ram_end:

;; writeFlashBuffer [Flash]
;;  Writes several bytes of memory to Flash
;; Inputs:
;;  DE: Address to write to
;;  HL: Address to read from (in RAM)
;;  BC: Length of data to write
;; Notes:
;;  Flash must be unlocked. Do not attempt to read your source data
;;  from Flash, you must load any data to be written into RAM. This
;;  will only *reset* bits of Flash.
writeFlashBuffer:
    push hl
    push bc
    push de
    push af
        ld a, i
        push af
            di
            push hl
            push de
            push bc
                ld hl, .ram
                ld de, flashFunctions
                ld bc, .ram_end - .ram
                ldir
            pop bc
            pop de
            pop hl
            jp flashFunctions
.return:
        pop af
        jp po, _
        ei
_:  pop af
    pop de
    pop bc
    pop hl
    ret
.ram:
.loop:
    ex de, hl
    ld a, (de)
    and (hl)
    ex af, af'
    ld a, 0xAA
    ld (0x0AAA), a    ; Unlock
    ld a, 0x55
    ld (0x0555), a    ; Unlock
    ld a, 0xA0
    ld (0x0AAA), a    ; Write command
    ex af, af'
    ld (hl), a        ; Data
    ex de, hl
    
.poll:
    ; This is completely wrong but doing it right crashes the fucking system
    xor (hl)
    bit 7, a
    jr z, .continue
    bit 5, a
    jr z, .poll
    ; Error, abort
    ld a, 0xF0
    ld (0), a
    jp .return
.continue:
    ld a, 0xF0
    ld (de), a ; Reset

    inc de
    inc hl
    dec bc

    xor a
    cp c
    jr nz, .loop
    cp b
    jr nz, .loop
    jp .return
.ram_end:

;; eraseSwapSector [Flash]
;;  Erases the swap sector.
;; Notes:
;;  Flash must be unlocked.
eraseSwapSector:
    push af
        ld a, swapSector
        call eraseFlashSector
    pop af
    ret

;; eraseFlashSector [Flash]
;;  Erases one sector of Flash (generally 4 pages of Flash, or 64K)
;;  by setting each byte to 0xFF.
;; Inputs:
;;  A: Any page within the target sector
;; Notes:
;;  Flash must be unlocked.
eraseFlashSector:
    push bc
    ld b, a
    push af
    ld a, i
    push af
    di
    ld a, b
    and 0b11111100
    push hl
    push de
    push bc
        ld hl, .ram
        ld de, flashFunctions
        ld bc, .ram_end - .ram
        ldir
    pop bc
    pop de
    pop hl
    jp flashFunctions
.return:
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    ret
    
.ram:
    setBankA
    ld a, 0xAA
    ld (0x0AAA), a ; Unlock
    ld a, 0x55
    ld (0x0555), a ; Unlock
    ld a, 0x80
    ld (0x0AAA), a ; Write command
    ld a, 0xAA
    ld (0x0AAA), a ; Unlock
    ld a, 0x55
    ld (0x0555), a ; Unlock
    ld a, 0x30
    ld (0x4000), a ; Erase
    ; Wait for chip
_:  ld a, (0)
    bit 7, a
    jp nz, .return
    bit 5, a
    jr z, -_
    ; Error, abort
    ld a, 0xF0
    ld (0x4000), a
    jp .return
.ram_end:

;; eraseFlashPage [Flash]
;;  Erases a single page of Flash.
;; Inputs:
;;  A: Target page
;; Notes:
;;  Flash must be unlocked. This is a very costly operation, and you
;;  may want to consider handling this logic yourself if you have to
;;  erase more than one page in a single sector
eraseFlashPage:
    push af
    push bc
        push af
            call copySectorToSwap
        pop af
        push af
            call eraseFlashSector
        pop af
        
        ld c, a
        and 0b1111100
        ld b, swapSector
        ; b is page in swap sector, a is page in target sector, c is target page
_:
        cp c
        jr z, .skipPage
        call copyFlashPage
.skipPage:
        inc b
        inc a
        push af
        ld a, b
        and 0b0000011
        or a
        jr z, .return
        pop af
        jr -_
.return:
        pop af
    pop bc
    pop af
    ret

;; copySectorToSwap [Flash]
;;  Copies a single sector of Flash to the swap sector.
;; Inputs:
;;  A: Any page within the sector to be copied
;; Notes:
;;  Flash must be unlocked.
copySectorToSwap:
    call eraseSwapSector

    push bc
    ld b, a
    push af
    ld a, i
    push af
    push de
    push hl
        di
        ld a, b
        and 0b11111100
        push hl
        push bc
        push de
            ld hl, .ram
            ld de, flashFunctions
            ld bc, .ram_end - .ram
            ldir
        pop de
        pop bc
        pop hl
#ifdef CPU15
        ; We'll move flashFunctions into bank 3 so that we can put both
        ; pages involved into bank 1 and 2 for a while
        push af
            ld a, 1
            out (0x05), a
        pop af
        jp flashFunctions + 0x4000
#else
        jp flashFunctions
#endif
.return:
#ifdef CPU15
    ; Switch us back to normal, with R:00 in bank 3 and R:01 in bank 2
    xor a
    out (0x05), a
    setBankB(0x81)
#endif
    pop hl
    pop de
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    ret
; On 15 MHz calcs, we can do this faster with clever mapping
; This is not possible on the TI-73 and TI-83+
#ifdef CPU15
.ram:
    setBankB
    setBankA(swapSector)

.page_loop:
    ld hl, 0x8000
    ld de, 0x4000
    ld bc, 0x4000
.inner_loop:
    ex de, hl
    ld a, (de)
    and (hl)
    ld (flashFunctions + 0x4000 + 0xFF), a
    ex af, af'
    ld a, 0xAA
    ld (0x0AAA), a
    ld a, 0x55
    ld (0x0555), a
    ld a, 0xA0
    ld (0x0AAA), a
    ex af, af'
    ld (hl), a

.poll:
    ld a, (flashFunctions + 0x4000 + 0xFF)
    xor (hl)
    bit 7, a
    jr z, .continue
    bit 5, a
    jr z, .poll
    ld a, (flashFunctions + 0x4000 + 0xFF)
    xor (hl)
    bit 7, a
    jr z, .continue
    ; Error, skip this byte
.continue:
    ex de, hl
    ld a, 0xF0
    ld (de), a
    
    inc de
    inc hl
    dec bc

    xor a
    cp c
    jr nz, .inner_loop
    cp b
    jr nz, .inner_loop

    getBankA
    inc a
    cp swapSector + 4
    jp z, .return
    setBankA
    getBankB
    inc a
    setBankB
    jr .page_loop
.ram_end:
#else ; TI-73, TI-83+
.ram:
    ld d, a
    setBankA
    ld e, swapSector

.page_loop:
    ld hl, 0x4000
    ld bc, 0x4000
.inner_loop:
    ld a, (hl) ; source sector
    ld (flashFunctions + 0xFF), a
    ex af, af'
    ld a, e ; swap sector
    setBankA
    ld a, 0xF0
    ld (0), a
    ld a, 0xAA
    ld (0x0AAA), a
    ld a, 0x55
    ld (0x0555), a
    ld a, 0xA0
    ld (0x0AAA), a
    ex af, af'
    ld (hl), a
.poll:
    ld a, (flashFunctions + 0xFF)
    xor (hl)
    bit 7, a
    jr z, .continue
    bit 5, a
    jr z, .poll
    ld a, (flashFunctions + 0xFF)
    bit 7, a
    jr z, .continue
    ; Error, skip this byte
.continue:
    ld a, 0xF0
    ld (hl), a
    
    inc hl
    dec bc

    ld a, d
    setBankA

    xor a
    cp c
    jr nz, .inner_loop
    cp b
    jr nz, .inner_loop

    inc d \ inc e
    ld a, d
    setBankA
    and 0b00000011
    jr nz, .page_loop
    jp .return
.ram_end:
#endif

;; copyFlashExcept [Flash]
;;  Copies one Flash page to another, but omits a certain range of bytes in increments of
;;  0x100 bytes.
;; Inputs:
;;  A: Destination page
;;  B: Source page
;;  H: High byte of address to stop copying at
;;  L: High byte of address to resume copying at
;; Notes:
;;  Flash must be unlocked and the destination page must be cleared.
;;  
;;  If you want to copy all but 0x6000 to 0x6500, set HL to 0x6065.
copyFlashExcept:
    ; Note: If we ever get short on space on page 0, we can merge this with copyFlashPage
    push de
    push bc
    ld d, a
    push af
    ld a, i
    push af
    di
    ld a, d
    
    push hl
    push de
        push af
        push bc
        push hl
        ld hl, .ram
#ifdef CPU15
        ld a, 1
        out (PORT_RAM_PAGING), a
        ; This routine can perform better on some models if we rearrange memory 
        ld de, flashFunctions + 0x4000
        ld bc, .ram_end - .ram
        ldir
#else
        ld de, flashFunctions
        ld bc, .ram_end - .ram
        ldir
#endif
        pop hl
        pop bc
        pop af
#ifdef CPU15
        jp flashFunctions + 0x4000
.return:
        xor a
        out (PORT_RAM_PAGING), a ; Restore correct memory mapping
#else
        jp flashFunctions
.retrurn:
#endif
    pop de
    pop hl
    
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    pop de
    ret
    
#ifdef CPU15
.ram:
    setBankA ; Destination
    ld a, b
    setBankB ; Source
    ld a, h
    ld (.skip_check_smc - .ram + flashFunctions + 0x4000 + 1), a
    ld a, l
    ld (.skip_apply_smc - .ram + flashFunctions + 0x4000 + 1), a
    ld (.skip_apply_smc_2 - .ram + flashFunctions + 0x4000 + 1), a
    
.preLoop:
    ld de, 0x8000
    ld hl, 0x4000
    ld bc, 0x4000
.loop:
.skip_check_smc:
    ld a, 0
    cp h
    jr z, .skip

.continue_loop:
    ld a, (de)
    ld (.smc - .ram + flashFunctions + 0x4000 + 1), a
    ld (_ + - .ram + flashFunctions + 0x4000 + 1), a
    ld a, 0xAA
    ld (0x0AAA), a    ; Unlock
    ld a, 0x55
    ld (0x0555), a    ; Unlock
    ld a, 0xA0
    ld (0x0AAA), a    ; Write command
.smc:
    ld a, 0
    ld (hl), a
    
_:  ld a, 0 ; also smc, don't optimize
    xor (hl)
    bit 7, a
    jr z, _
    bit 5, a
    jr z, -_
    jr _ ; See note on copySectorToSwap
    ; Error, abort
    ld a, 0xF0
    ld (0), a
    setBankB(0x81)
    jp .return
_:
    inc de
    inc hl
    dec bc

    ld a, b
    or c
    jr nz, .loop
    setBankB(0x81)
    jp .return
.skip:
.skip_apply_smc:
    ld a, 0
    sub h
    neg
    add a, b
    ld b, a
.skip_apply_smc_2:
    ld h, 0
    ld d, h
    res 6, d
    set 7, d ; Bump up to 0x8000 range
    jr .continue_loop
.ram_end:
#else ; Models that don't support placing RAM page 01 in bank 3 (much slower)
.ram:
    ld e, b
    
    ld (flashFunctions + flashFunctionSize - 1), a
.preLoop:
    ld hl, 0x4000
    ld bc, 0x4000
.loop:
    push af
        ld a, e
        setBankA ; The inefficiency on this model comes from swapping pages during the loop
        ld d, (hl)
    pop af
    setBankA
    ; copy D to (HL)
    ld a, 0xAA
    ld (0x0AAA), a    ; Unlock
    ld a, 0x55
    ld (0x0555), a    ; Unlock
    ld a, 0xA0
    ld (0x0AAA), a    ; Write command
    ld (hl), d        ; Data
    
    ld a, d
_:  cp (hl)
    jr nz, -_ ; Does this work?
    
    dec bc
    inc hl
    
    ld a, b
    or a
    ld a, (flashFunctions + flashFunctionSize - 1)
    jr nz, .loop
    ld a, c
    or a
    ld a, (flashFunctions + flashFunctionSize - 1)
    jr nz, .loop
    ret
.ram_end:
#endif

;; copyFlashPage [Flash]
;;  Copies one page of Flash to another.
;; Inputs:
;;  A: Destination page
;;  B: Source page
;; Notes:
;;  Flash must be unlocked and the desination page must be cleared.
copyFlashPage:
    push de
    push bc
    ld d, a
    push af
    ld a, i
    push af
    di
    ld a, d
    
    push hl
    push de
        push af
        push bc
        ld hl, .ram
#ifdef CPU15
            ld a, 1
            out (PORT_RAM_PAGING), a
            ; This routine can perform better on some models if we rearrange memory 
            ld de, flashFunctions + 0x4000
            ld bc, .ram_end - .ram
            ldir
#else
        ld de, flashFunctions
        ld bc, .ram_end - .ram
        ldir
#endif
        pop bc
        pop af
#ifdef CPU15
        jp flashFunctions + 0x4000
.return:
        xor a
        out (PORT_RAM_PAGING), a ; Restore correct memory mapping
#else
        jp flashFunctions
.return:
#endif
    pop de
    pop hl
    
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    pop de
    ret
    
#ifdef CPU15
.ram:
    setBankA ; Destination
    ld a, b
    setBankB ; Source
    
.preLoop:    
    ld de, 0x8000
    ld hl, 0x4000
    ld bc, 0x4000
.loop:
    ld a, (de)
    ld (.smc - .ram + flashFunctions + 0x4000 + 1), a
    ld (_ + - .ram + flashFunctions + 0x4000 + 1), a
    ld a, 0xAA
    ld (0x0AAA), a    ; Unlock
    ld a, 0x55
    ld (0x0555), a    ; Unlock
    ld a, 0xA0
    ld (0x0AAA), a    ; Write command
.smc:
    ld a, 0
    ld (hl), a
    
_:  ld a, 0 ; also smc, don't optimize
    xor (hl)
    bit 7, a
    jr z, _
    bit 5, a
    jr z, -_
    jr _ ; See note on copySectorToSwap
    ; Error, abort
    ld a, 0xF0
    ld (0), a
    setBankB(0x81)
    jp .return
_:
    inc de
    inc hl
    dec bc

    ld a, b
    or c
    jr nz, .loop
    setBankB(0x81)
    jp .return
.ram_end:
#else ; Models that don't support placing RAM page 01 in bank 3 (much slower)
.ram:
    ; Called with destination in A, target in B
    ld (.smc2 - .ram + flashFunctions + 1), a
    ld a, b
    ld (.smc - .ram + flashFunctions + 1), a
.preLoop:
    ld hl, 0x4000
    ld bc, 0x4000
.loop:
.smc:
    ld a, 0
    setBankA
    ld d, (hl)
.smc2:
    ld a, 0
    setBankA
    ; copy D to (HL)
    ld a, 0xAA
    ld (0x0AAA), a    ; Unlock
    ld a, 0x55
    ld (0x0555), a    ; Unlock
    ld a, 0xA0
    ld (0x0AAA), a    ; Write command
    ld (hl), d        ; Data
    
    ld a, d
_:  xor (hl)
    bit 7, a
    jr z, _
    bit 5, a
    jr z, -_
    jr _ ; See note on copySectorToSwap
    ; Error, abort
    ld a, 0xF0
    ld (0), a
    jp .return
    
_:  inc hl
    dec bc
    
    ld a, b
    or c
    jr nz, .loop
    jp .return
.ram_end:
#endif
