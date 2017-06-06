;; itofp [FP Math]
;;  Converts a 32-bit unsigned integer into a floating-point
;;  binary coded decimal format and stores it to the buffer at HL.
;; Inputs:
;;  ACIX: Unsigned integer
;;  HL: Pointer to 9-byte destination buffer
;; Notes:
;;  The result is in the following format:
;;  * 1 byte flags, currently only a sign bit as the MSB
;;  * 1 byte signed exponent, normalized to 0x80 instead of 0
;;  * 7 byte mantissa, BCD encoded with two digits per byte
itofp:
    ; Modified version of itostr
    push hl
    push ix
    push bc
    push af
    push de
    push iy
        ; Move HL to IY for convenience
        push hl \ pop iy
        ; Clear the flags
        ld (iy), 0
        ld b, 0     ; Our digit counter
.loop:
        ; Check if ACIX is zero
        push bc
            ; If a is not zero
            cp 0
            jp nz, .notZero
            ; If c is not zero
            ld b, a
            ld a, c
            cp 0
            ld a, b
            jp nz, .notZero
            ; If ixh is not zero
            ld b, a
            ld a, ixh
            cp 0
            ld a, b
            jp nz, .notZero
            ; If ixl is not zero
            ld b, a
            ld a, ixl
            cp 0
            ld a, b
.notZero:
        pop bc
        ; If ACIX is down to zero, exit loop
        jr z, _
        ; Divide ACIX by 10
        push iy
            ld iyh, b
            ld de, 10
            call div32By16
            ld b, iyh
        pop iy
        push hl     ; Push the remainder to the stack
        inc b       ; Increment our digit counter
        jr .loop
_:
        ; Write a zero if ACIX is zero
        ld a, b
        cp 0
        jr nz, .writeExp
        ld (iy), 0
        inc iy
.writeExp:
        ld a, b
        add a, 0x7f
        ld (iy+1), a
.writeMantissa:
        ld a, b
        cp 0
        jr z, _     ; If our digit counter is zero, exit loop
        pop de      ; Pop our digit from the stack
        dec b       ; Decrement our digit counter
        ld a, b
        cp 0
        jr z, .lastDigit    ; If we have an odd number of digits, don't pop
        pop hl      ; Pop our digit from the stack
        dec b       ; Decrement our digit counter
        jr .writeDigits
.lastDigit:
        ld hl, 0     ; Load a zero instead if there are no more digits
.writeDigits:
        ; Pack the two digits
        ld a, e
        rrca
        rrca
        rrca
        rrca
        or l
        ; Write the digits
        ld (iy+2), a
        inc iy
        jr .writeMantissa
_:
    pop iy
    pop de
    pop af
    pop bc
    pop ix
    pop hl
    ret
