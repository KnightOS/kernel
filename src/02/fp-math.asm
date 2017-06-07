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

;; fpAdd [FP Math]
;;  Adds the two floating point numbers at BC and DE and
;;  stores the result at HL.
;; Inputs:
;;  BC, DE: Pointers to floating point operands
;;  HL: Pointer to destination buffer
fpAdd:
    push ix
    push iy
    push de
    push bc
    push hl
        ; Move operands to index registers for convenience
        push bc \ pop ix
        push de \ pop iy
        ; Make sure that iy has the smaller exponent
        ld a, (ix + 1)
        ld b, (iy + 1)
        cp b
        jr nc, .setupLoop
        ; iy is larger, so swap with ix
        push ix \ push iy \ pop ix \ pop iy
.setupLoop:
        ; Start at the end of the buffers
        ld bc, 8
        add ix, bc
        add iy, bc
        add hl, bc
        ld b, 7
        ; Clear the carry flag for the first digit
        scf \ ccf
.loop:
        ld a, (ix)
        ld c, (iy)
        adc a, c
        daa     ; Adjust the addition for BCD
        ld (hl), a
        dec ix
        dec iy
        dec hl
        ; TODO: handle exponent/digit shifts
        ; TODO: handle sign
        djnz .loop
    pop hl
    pop bc
    pop de
    pop iy
    pop ix
    ret

;; fpSub [FP Math]
;;  Subtracts the floating point numbers at DE from BC and
;;  stores the result at HL.
;; Inputs:
;;  BC, DE: Pointers to floating point operands
;;  HL: Pointer to destination buffer
fpSub:
    ex de, hl
    call fpNeg
    ex de, hl
    call fpAdd
    ret

;; fpNeg [FP Math]
;;  Negates the floating point number at HL.
;; Inputs:
;;  HL: Pointer to floating point operand
fpNeg:
    push af
    ld a, (hl)
    xor 0x80
    ld (hl), a
    pop af
    ret

;; fpCompare [FP Math]
;;  Compares the two floating point numbers at BC and DE.
;; Inputs:
;;  BC, DE: Pointers to floating point operands
;; Output:
;;  Same as z80 CP instruction.
fpCompare:
    push bc
    push ix
    push iy
    push hl
        ; Move operands to index registers for convenience
        push bc \ pop ix
        push de \ pop iy
        ; Save A
        ld l, a
        ; Compare signs
        ld a, (ix)
        and 0x80
        ld b, a
        ld a, (iy)
        and 0x80
        cp b
        jr nz, .end     ; Operands have opposite signs
        ; Check if both operands are negative
        and 0x80
        jr z, .noSwap
        ; Both operands are negative, so swap them to ensure correct comparison
        push ix \ push iy \ pop ix \ pop iy
.noSwap:
.macro fpCompareIter
        inc ix
        inc iy
        ld a, (ix)
        ld b, (iy)
        cp b
        jr nz, .end
.endmacro
        ; Compare exponents
        fpCompareIter
        ; Compare mantissas
        fpCompareIter
        fpCompareIter
        fpCompareIter
        fpCompareIter
        fpCompareIter
        fpCompareIter
        fpCompareIter
.undefine fpCompareIter
.end:
        ; Restore A
        ld a, l
    pop hl
    pop iy
    pop ix
    pop bc
    ret
