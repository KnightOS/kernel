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
    push bc
    ld b, a
    push af
    ld a, i
    push af
    di
    ld a, b

    push hl
    push de
    push bc
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
        call flashFunctions
    pop bc
    pop de
    pop hl
    
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    ret

; Flash operations must be done from RAM
.ram:
    and (hl) ; Ensure that no bits are set
    ld b, a
    ld a, 0xAA
    ld (0x0AAA), a    ; Unlock
    ld a, 0x55
    ld (0x0555), a    ; Unlock
    ld a, 0xA0
    ld (0x0AAA), a    ; Write command
    ld (hl), b        ; Data
    
    ; Wait for chip
_:  ld a, b
    xor (hl)
    bit 7, a
    jr z, .done
    bit 5, (hl)
    jr z, -_
    ; Error, abort
.done:
    ld (hl), 0xF0
    ret
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
#ifdef COLOR
    ; TODO: Fix this crap
    ret
#endif
    push af
    ld a, i
    push af
    di

    push hl
    push de
    push bc
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
        call flashFunctions
    pop bc
    pop de
    pop hl
    
    pop af
    jp po, _
    ei
_:  pop af
    ret
    
.ram:
.loop:
    ld a, 0xAA
    ld (0x0AAA), a    ; Unlock
    ld a, 0x55
    ld (0x0555), a    ; Unlock
    ld a, 0xA0
    ld (0x0AAA), a    ; Write command
    ld a, (hl)
    ld (de), a        ; Data
    
    inc de
    dec bc
    
_:  xor (hl)
    bit 7, a
    jr z, _
    bit 5, a
    jr z, -_
    ; Error, abort
    ld a, 0xF0
    ld (0), a
    ret
_:  inc hl
    ld a, 0xF0
    ld (de), a
    xor a
    cp c
    jr nz, .loop
    cp b
    jr nz, .loop
    ret
.ram_end:

;; eraseSwapSector [Flash]
;;  Erases the swap sector.
;; Notes:
;;  Flash must be unlocked.
eraseSwapSector:
    ld a, swapSector
    call eraseFlashSector
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
    ld a, i
    push af
    di
    ld a, b

    push hl
    push de
    push bc
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
        call flashFunctions
    pop bc
    pop de
    pop hl
    
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
    ; Wait for 0xcip
_:  ld a, (0x4000)
    bit 7, a
    ret nz
    bit 5, a
    jr z, -_
    ; Error, abort
    ld a, 0xF0
    ld (0x4000), a
    ret
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
        and 0b111111100
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
        and 0b000000011
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
    push af
    call eraseSwapSector
    pop af

    push bc
    ld b, a
    push af
    ld a, i
    ld a, i
    push af
    di
    ld a, b

    and 0b011111100 ; Get the sector for the specified page
    
    push hl
    push de
        ld hl, .ram
#ifdef CPU15
        push af
            ld a, 1
            out (PORT_RAM_PAGING), a
        
            ld de, flashFunctions + 0x4000 ; By rearranging memory, we can make the routine perform better
            ld bc, .end - .ram
            ldir
        pop af
#else
        ld de, flashFunctions
        ld bc, .end - .ram
        ldir
#endif
#ifdef CPU15
        ld hl, 0x4000
        add hl, sp
        ld sp, hl
        call flashFunctions + 0x4000
        xor a
        out (PORT_RAM_PAGING), a ; Restore correct memory mapping
        ld hl, 0
        add hl, sp
        ld bc, 0x4000
        or a
        sbc hl, bc
        ld sp, hl
#else
        call flashFunctions
#endif
    pop de
    pop hl
    
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    ret
    
#ifdef CPU15
.ram:
    setBankB
    setBankA(swapSector)
    
.preLoop:    
    ld hl, 0x8000
    ld de, 0x4000
    ld bc, 0x4000
.loop:
    ld a, 0xAA
    ld (0x0AAA), a    ; Unlock
    ld a, 0x55
    ld (0x0555), a    ; Unlock
    ld a, 0xA0
    ld (0x0AAA), a    ; Write command
    ld a, (hl)
    ld (de), a        ; Data
    inc de
    dec bc
    
_:  xor (hl)
    bit 7, a
    jr z, _
    bit 5, a
    jr z, -_
    ; Error, abort
    ld a, 0xF0
    ld (0), a
    setBankB(0x81)
    ret
_:
    inc hl
    ld a, b
    or a
    jr nz, .loop
    ld a, c
    or a
    jr nz, .loop
    
    getBankB
    inc a
    setBankB
    
    getBankA
    inc a
    setBankA
    and 0b000000011
    or a
    jr nz, .preLoop
    
    ld a, 0x81
    setBankB
    ret
.end:

#else ; Models that don't support placing RAM page 01 in bank 3 (mu0xc slower)
.ram:
    ld e, a
    
    ld a, swapSector
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
    
    inc e
    ld a, (flashFunctions + flashFunctionSize - 1)
    inc a
    ld (flashFunctions + flashFunctionSize - 1), a
    and 0b000000011
    or a
    ld a, (flashFunctions + flashFunctionSize - 1)
    jr nz, .preLoop
    ret
.end:
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
        ld hl, 0x4000
        add hl, sp
        ld sp, hl
        call flashFunctions + 0x4000
        xor a
        out (PORT_RAM_PAGING), a ; Restore correct memory mapping
        ld hl, 0
        add hl, sp
        ld bc, 0x4000
        or a
        sbc hl, bc
        ld sp, hl
#else
        call flashFunctions
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
    ld hl, 0x8000
    ld de, 0x4000
    ld bc, 0x4000
.loop:
    ld a, 0xAA
    ld (0x0AAA), a    ; Unlock
    ld a, 0x55
    ld (0x0555), a    ; Unlock
    ld a, 0xA0
    ld (0x0AAA), a    ; Write command
    ld a, (hl)
    ld (de), a        ; Data
    inc de
    dec bc
    
_:    xor (hl)
    bit 7, a
    jr z, _
    bit 5, a
    jr z, -_
    ; Error, abort
    ld a, 0xF0
    ld (0), a
    setBankB(0x81)
    ret
_:
    inc hl
    ld a, b
    or a
    jr nz, .loop
    ld a, c
    or a
    jr nz, .loop
    
    setBankB(0x81)
    ret
.ram_end:
#else ; Models that don't support placing RAM page 01 in bank 3 (mu0xc slower)
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
