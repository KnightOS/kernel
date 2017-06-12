;; itofp [FP Math]
;;  Converts a 32-bit unsigned integer into a floating-point
;;  binary coded decimal format and stores it to the buffer at HL.
;; Inputs:
;;  ACIX: Unsigned integer
;;  HL: Pointer to 9-byte destination buffer
;; Output:
;;  HL: Pointer to result
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

;; strtofp [FP Math]
;;  Converts an ASCII-encoded signed decimal into a floating-point
;;  binary coded decimal format and stores it to the buffer at HL.
;; Inputs:
;;  IX: Pointer to string
;;  HL: Pointer to 9-byte destination buffer
;; Output:
;;  HL: Pointer to result
;;  Z: Set on success, reset on error
;; Notes:
;;  The result is in the following format:
;;  * 1 byte flags, currently only a sign bit as the MSB
;;  * 1 byte signed exponent, normalized to 0x80 instead of 0
;;  * 7 byte mantissa, BCD encoded with two digits per byte
;;
;;  Only the first 14 significant digits are converted. The rest are truncated
;;  but still used for exponent calculation.
;;
;;  In case of error, the destination buffer's contents are undefined.
;;
;;  The destination buffer must be zeroed before calling strtofp.
strtofp:
    push ix
    push hl
    push bc
    push de
    push iy
    push af
        ; Store HL into IY for convenience
        push hl \ pop iy
        ; Check for a negative sign at the beginning
        ld a, (ix)
        cp '-'
        jr nz, _
        ld (hl), 0x80
        inc ix
_:
        inc hl
        inc hl
        ld b, 14    ; Remaining digits counter
        ld d, 0     ; Parsing flags
        ld e, 0x80  ; Place value counter
.loop:
        ; Check if we are at the end of the buffer
        xor a
        cp b
        jr z, .loopEnd
        ; Check if we've reached a null terminator
        ld a, (ix)
        or a
        jr z, .loopEnd
        cp '.'
        jr nz, _
        ; Make sure we haven't already seen a decimal point
        bit 0, d
        jr nz, .error
        set 0, d
        jr .loopCont
_:
        ; Error on non-numeric characters
        cp '9' + 1
        jr nc, .error
        sub '0'
        jr c, .error
        jr nz, _
        ; Handle zero
        bit 1, d
        jr z, .loopCont     ; Skip leading zero
_:
        set 1, d    ; Finished with all leading zeroes
        rld         ; Shift the digit into the current BCD byte
        bit 0, b    ; Only increment HL every other digit
        jr z, .loopIter
        inc hl
.loopIter:
        ; Adjust place value for digits before decimal
        ld a, d
        cp 0x02
        jr nz, _
        inc e
_:
        inc ix
        djnz .loop
        jr .loopEnd
.loopCont:
        ; Adjust place value for leading zeroes after decimal
        ld a, d
        cp 0x01
        jr nz, _
        dec e
_:
        inc ix
        jr .loop
.loopEnd:
        ; Shift the last digit if there were an odd number
        bit 0, b
        jr z, _
        xor a
        rld
_:
        ; Check if there may be more place values that need counted
        ld a, b
        or a
        jr nz, .remainderLoopEnd
        bit 0, d
        jr nz, .remainderLoopEnd
.remainderLoop:
        ; Count remaining place values
        ld a, (ix)
        or a
        jr z, .remainderLoopEnd
        cp '.'
        jr z, .remainderLoopEnd
        cp '9' + 1
        jr nc, .error
        cp '0'
        jr c, .error
        inc e
        inc ix
        jr .remainderLoop
.remainderLoopEnd:
        ; Correct for one's place value
        ld a, 0x80
        cp e
        jr nc, _
        dec e
_:
        ; Save the place value counter into the exponent byte
        ld (iy + 1), e
        pop af
        cp a    ; Set Z flag
        jr .exit
.error:
        pop af
        ; Save A
        ld b, a
        or 1    ; Reset Z flag
        ; Restore A
        ld a, b
.exit:
    pop iy
    pop de
    pop bc
    pop hl
    pop ix
    ret

;; fpAdd [FP Math]
;;  Adds the two floating point numbers.
;; Inputs:
;;  IX, IY: Pointers to operands
;;  HL: Pointer to destination buffer
;; Output:
;;  HL: Pointer to result
fpAdd:
    push ix
    push iy
    push hl
    push af
    push bc
        ; Make sure that IY has the smaller exponent
        ld a, (ix + 1)
        cp (iy + 1)
        jr nc, _
        ; IY is larger, so swap with IX
        push ix \ push iy \ pop ix \ pop iy
_:
        ; Set the result's exponent to IX's for now
        inc hl
        ld (hl), a
        dec hl
        ; Invert negative operands using 10's complement
.macro fpAddTensComplIter(r)
        ld a, 0x99
        sub (r)
        ld (r), a
.endmacro
        bit 7, (ix)
        jr z, _     ; Not negative
        fpAddTensComplIter(ix + 2)
        fpAddTensComplIter(ix + 3)
        fpAddTensComplIter(ix + 4)
        fpAddTensComplIter(ix + 5)
        fpAddTensComplIter(ix + 6)
        fpAddTensComplIter(ix + 7)
        fpAddTensComplIter(ix + 8)
_:
        bit 7, (iy)
        jr z, _     ; Not negative
        fpAddTensComplIter(iy + 2)
        fpAddTensComplIter(iy + 3)
        fpAddTensComplIter(iy + 4)
        fpAddTensComplIter(iy + 5)
        fpAddTensComplIter(iy + 6)
        fpAddTensComplIter(iy + 7)
        fpAddTensComplIter(iy + 8)
.undefine fpAddTensComplIter
_:
        ; Start at the end of the buffer
        ld bc, 8
        add hl, bc
        ; Clear the carry flag if operands have the same sign, else set it
        ld a, (ix)
        and 0x80
        ld b, a
        ld a, (iy)
        and 0x80
        cp b
        scf
        ; jr nz, _  ; TODO: find a way to only add 1 when it will overflow
        ccf
_:
.macro fpAddSumIter(x, y)
        ld a, (x)
        adc a, (y)
        daa     ; Adjust the addition for BCD
        ld (hl), a
        dec hl
.endmacro
        fpAddSumIter(ix + 8, iy + 8)
        fpAddSumIter(ix + 7, iy + 7)
        fpAddSumIter(ix + 6, iy + 6)
        fpAddSumIter(ix + 5, iy + 5)
        fpAddSumIter(ix + 4, iy + 4)
        fpAddSumIter(ix + 3, iy + 3)
        fpAddSumIter(ix + 2, iy + 2)
        ; TODO: handle exponent/digit shifts
.undefine fpAddSumIter
        ; Handle carry
        jr nc, _
        ld a, 1
.macro fpAddCarryIter
        inc hl
        rrd
.endmacro
        fpAddCarryIter
        fpAddCarryIter
        fpAddCarryIter
        fpAddCarryIter
        fpAddCarryIter
        fpAddCarryIter
        fpAddCarryIter
.undefine fpAddCarryIter
        ld bc, -7
        add hl, bc
        inc (hl)
_:
    pop bc
    pop af
    pop hl
    pop iy
    pop ix
    ret

;; fpSub [FP Math]
;;  Subtracts the two floating point numbers.
;; Inputs:
;;  IX: Pointer to operand 1 (minuend)
;;  IY: Pointer to operand 2 (subtrahend)
;;  HL: Pointer to destination buffer
;; Output:
;;  HL: Pointer to result
fpSub:
    push af
    ld a, (iy)
    xor 0x80
    ld (iy), a
    pop af
    jp fpAdd

;; fpNeg [FP Math]
;;  Negates the floating point number at HL.
;; Input:
;;  IX: Pointer to operand
;; Output:
;;  IX: Pointer to result
fpNeg:
    push af
    ld a, (ix)
    xor 0x80
    ld (ix), a
    pop af
    ret

;; fpCompare [FP Math]
;;  Compares the two floating point numbers.
;; Inputs:
;;  IX, IY: Pointer to operands
;; Output:
;;  Same as z80 CP instruction.
fpCompare:
    push ix
    push iy
    push bc
        ; Save A
        ld c, a
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
        jr z, _
        ; Both operands are negative, so swap them to ensure correct comparison
        push ix \ push iy \ pop ix \ pop iy
_:
.macro fpCompareIter(x, y)
        ld a, (x)
        cp (y)
        jr nz, .end
.endmacro
        ; Compare exponents
        fpCompareIter(ix + 1, iy + 1)
        ; Compare mantissas
        fpCompareIter(ix + 2, iy + 2)
        fpCompareIter(ix + 3, iy + 3)
        fpCompareIter(ix + 4, iy + 4)
        fpCompareIter(ix + 5, iy + 5)
        fpCompareIter(ix + 6, iy + 6)
        fpCompareIter(ix + 7, iy + 7)
        fpCompareIter(ix + 8, iy + 8)
.undefine fpCompareIter
.end:
        ; Restore A
        ld a, c
    pop bc
    pop iy
    pop ix
    ret
