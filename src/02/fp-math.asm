; FP Math TODO:
; 1. Addition and subtraction
; 2. Multiplication and division
; 3. Miscellaneous math operations:
;   - Roots/powers
;   - Logarithms
;   - Trigonometry
;   - etc.
; 4. Conversion to and from IEEE 754 floats/doubles



;; itofp [FP Math]
;;  Converts a 32-bit unsigned integer into a floating-point
;;  binary coded decimal format and stores it to the buffer at HL.
;; Inputs:
;;  ACIX: Unsigned integer
;;  HL: Pointer to 9-byte destination buffer
;; Notes:
;;  The result is in the following format:
;;  * 1 byte flags:
;;      - 7: Sign bit
;;      - 4-6: Reserved for kernel
;;      - 0-3: Available for program use
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
            or a
            jp nz, .notZero
            ; If c is not zero
            ld b, a
            ld a, c
            or a
            ld a, b
            jp nz, .notZero
            ; If ixh is not zero
            ld b, a
            ld a, ixh
            or a
            ld a, b
            jp nz, .notZero
            ; If ixl is not zero
            ld b, a
            ld a, ixl
            or a
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
        or a
        jr nz, .writeExp
        ld (iy), 0
        inc iy
.writeExp:
        ld a, b
        add a, 0x7f
        ld (iy+1), a
.writeMantissa:
        ld a, b
        or a
        jr z, _     ; If our digit counter is zero, exit loop
        pop de      ; Pop our digit from the stack
        dec b       ; Decrement our digit counter
        ld a, b
        or a
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
;;  Z: Set on success, reset on error
;; Notes:
;;  See `itofp` for the result format.
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
        ld b, 14      ; Remaining digits counter
        ld de, 0x0080 ; Parser flags and place value counter
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

;; fptostr [FP Math]
;;  Converts a floating point number into an ASCII-encoded decimal string.
;; Inputs:
;;  IX: Pointer to floating point number
;;  HL: Pointer to destination buffer
;;  A: Flags / digits
;; Notes:
;;  The destination buffer must be at least 20 characters in length.
;;
;;  The most significant nibble of A should be set to flags. FP_STR_*
;;  macros in kernel.inc can be ORed and stored in A for convenience.
;;
;;  The least significant nibble of A is the number of digits to display
;;  after the decimal: 0-9 for that many digits, or 0xF for as many are
;;  non-zero.
;;
;;  Examples:
;;  All nonzero decimals in scientific notation, digits grouped with ',':
;;    ld a, FP_GROUP_DIGITS | FP_DISP_SCIENTIFIC | 0xF
;;  5 fixed digits, digits grouped with '.' and decimal as ',':
;;    ld a, FP_GROUP_DIGITS | FP_STR_INV_PUNC | 5
; TODO:
;  * Rounding last digit - buggy, currently abandoned
;  * Never show exponent if significand is 0 - not started
.macro fptostrIter1(reg)
        ; Output the first digit in the byte pointed to by reg
        ld a, (reg)
        rrca
        rrca
        rrca
        rrca
        and 0x0F
        add a, '0'
        ld (hl), a
        inc hl
.endmacro
.macro fptostrIter2(reg)
        ; Output the second digit in the byte pointed to by reg
        ld a, (reg)
        and 0x0F
        add a, '0'
        ld (hl), a
        inc hl
.endmacro
.macro fptostrIter1Round(reg)
        ; Like fptostrIter1 but it rounds the last digit
        ld a, (reg)
        rrca
        rrca
        rrca
        rrca
        and 0x0F
        dec b
        jr nz, .fptostrIter1RoundSkip
        ld c, a
        ld a, (reg)
        and 0x0F
        cp 5
        jr c, .fptostrIter1RoundSkip
        ld a, c
        inc a
.fptostrIter1RoundSkip:
        inc b
        add a, '0'
        ld (hl), a
        inc hl
.endmacro
.macro fptostrIter2Round(reg)
        ; Like fptostrIter2 but it rounds the last digit
        ld a, (reg)
        and 0x0F
        dec b
        jr nz, .fptostrIter2RoundSkip
        ld c, a
        inc reg
        ld a, (reg)
        dec reg
        rrca
        rrca
        rrca
        rrca
        and 0x0F
        cp 5
        jr c, .fptostrIter2RoundSkip
        ld a, c
        inc a
.fptostrIter2RoundSkip:
        inc b
        add a, '0'
        ld (hl), a
        inc hl
.endmacro
.macro fptostrI18N(normal, alt)
        ; Output a different character depending on the bit 7 flag
        ex (sp), hl
        bit 7, h
        ex (sp), hl
        jr nz, $+6
        ld (hl), normal
        jr $+4
        ld (hl), alt
        inc hl
.endmacro
.macro fptostrInsertPVSep
        ; Output a place value separator depending on the bit 6 flag
        pop af \ push af
        bit 6, a
        jr z, ++_
        push de
            ld d, b
            dec d
            ld e, 3
            call div8By8
            or a
            jr nz, _
            or d
            jr z, _
        pop de
        fptostrI18N(',', '.')
        jr ++_
_:
        pop de
_:
.endmacro

fptostr:
    push hl
    push bc
    push de
    push af
        ; Check which mode to use
        bit 4, a
        jp nz, _
        ld a, (ix + 1)
        cp 0x8a
        jp nc, _
        cp 0x7d
        jp c, _
        ; Normal mode
        pop af \ push af
        jr ++_
_:
        ; Scientific mode
        pop af \ push af
        set 4, a
        res 5, a
_:
        push af
            ; Negative sign
            ld a, (ix)
            and 0x80
            jr z, _
            ld (hl), '-'
            inc hl
_:
            ; Calculate number of digits to display
            pop af \ push af
            bit 4, a
            jr z, _
            ; Scientific notation
            ld a, 1
            jr .beginIPart
_:
            ; Normal mode
            ld a, (ix + 1)
            cp 0x80
            jr c, _
            sub 0x7F
            jr .beginIPart
_:
            ; Negative exponent
            ld (hl), '0'
            inc hl
            push ix \ pop de
            inc de \ inc de
            ld c, 10    ; Should be 9, but that causes it to skip the last digit
            xor a
            jp .doneWithIPart
.beginIPart:
            ld b, a     ; Number of pre-decimal digits
            ld a, 10
            sub b
            ld c, a     ; (Maximum) number of post-decimal digits
            push ix \ pop de
            inc de \ inc de
.iPartLoop:
            fptostrIter1(de)
            fptostrInsertPVSep
            dec b
            ld a, 1     ; Mark that this byte isn't complete yet
            jr z, .doneWithIPart
            fptostrIter2(de)
            fptostrInsertPVSep
            inc de
            xor a       ; Mark that this byte is complete
            djnz .iPartLoop
.doneWithIPart:
            ; Check if we are using fixed point or not
            push af
                inc sp \ inc sp
                pop af \ push af
                dec sp \ dec sp
                and 0x0F
                jr z, ++_
                cp 10
                jr nc, +++_
                cp c
                jr nc, _
                ld c, a
_:
                xor a
                or c
                jr z, _
            pop af
            jr .skipFloating
_:
            pop af
            jr .end
_:
            pop af
            ; Calculate how many fractional digits there are
            push af
            push hl
            push de
                ; Escape early if there isn't room for fractional digits
                xor a
                or c
                jr z, _
                push ix \ pop hl
                ld de, 6    ; Ignore last 2 bytes for display purposes
                add hl, de
.countFractionalDigitsLoop:
                ld a, (hl)
                and 0x0F
                jr nz, ++_
                dec c
                jr z, _
                ld a, (hl)
                and 0xF0
                jr nz, ++_
                dec hl
                dec c
                jr nz, .countFractionalDigitsLoop
_:
            pop de
            pop hl
            pop af
            jp .end
_:
            pop de
            pop hl
            pop af
.skipFloating:
            fptostrI18N('.', ',')
            ; Check if we need to add leading zeroes
            push af
                ; Make sure we aren't in scientific notation mode
                inc sp \ inc sp
                pop af \ push af
                dec sp \ dec sp
                bit 4, a
                jr nz, _
                ; Check the exponent
                ld a, (ix + 1)
                sub 0x80
                jr nc, _
                neg
                dec a
                jr z, _
                ld b, a
.leadingZeroLoop:
                ld (hl), '0'
                inc hl
                dec c
                djnz .leadingZeroLoop
_:
            pop af
            ld b, c
            dec a
            jr z, .fPartLoopHalf
.fPartLoop:
            fptostrIter1(de)
            ; fptostrIter1Round(de)
            dec b
            jr z, .end
.fPartLoopHalf:
            fptostrIter2(de)
            ; fptostrIter2Round(de)
            inc de
            djnz .fPartLoop
.end:
        ; Check if need to include exponent or not
        pop af
        bit 4, a
        jr z, .terminate
        ld (hl), 'E'
        inc hl
        ; Exponent
        ld a, (ix + 1)
        sub 0x80
        jr nc, _
        ld (hl), '-'
        inc hl
        neg
_:
        ld d, a
        ld e, 10
        ld b, 0     ; Exponent digit counter
.calcExponentLoop:
        call div8By8
        add a, '0'
        push af
        inc b
        ld a, d
        or a
        jr nz, .calcExponentLoop
.writeExponentLoop:
        pop af
        ld (hl), a
        inc hl
        djnz .writeExponentLoop
.terminate:
        ; Null terminator
        ld (hl), 0
    pop af
    pop de
    pop bc
    pop hl
    ret
.undefine fptostrIter1
.undefine fptostrIter2
.undefine fptostrIter1Round
.undefine fptostrIter2Round
.undefine fptostrI18N
.undefine fptostrInsertPVSep

;; fpLdConst [FP Math]
;;  Loads a floating point constant specified by A into HL.
;; Input:
;;  A: Constant to load, use FP_* macros from kernel.inc
;;  HL: Pointer to destination buffer
fpLdConst:
    push af
    push bc
    push de
    push hl
        ex de, hl
        ld hl, .lookupTable
        ld bc, 9
.macro fpLdConstIter
        add hl, bc
        dec a
        jr z, _
.endmacro
        or a
        jr z, _
        fpLdConstIter
        fpLdConstIter
        fpLdConstIter
        fpLdConstIter
        fpLdConstIter
        fpLdConstIter
        fpLdConstIter
        fpLdConstIter
        fpLdConstIter
.undefine fpLdConstIter
        ; None matched - return unchanged
        jr .end
_:
        ldir
.end:
    pop hl
    pop de
    pop bc
    pop af
    ret

.lookupTable:
    ; 0
    .db 0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ; 1
    .db 0x00, 0x80, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ; pi
    .db 0x00, 0x80, 0x31, 0x41, 0x59, 0x26, 0x53, 0x58, 0x98
    ; pi/2
    .db 0x00, 0x80, 0x15, 0x70, 0x79, 0x63, 0x26, 0x79, 0x49
    ; pi/4
    .db 0x00, 0x7F, 0x78, 0x53, 0x98, 0x16, 0x33, 0x97, 0x45
    ; pi/180
    .db 0x00, 0x7E, 0x17, 0x45, 0x32, 0x92, 0x51, 0x99, 0x43
    ; 180/pi
    .db 0x00, 0x81, 0x57, 0x29, 0x57, 0x79, 0x51, 0x30, 0x82
    ; e
    .db 0x00, 0x80, 0x27, 0x18, 0x28, 0x18, 0x28, 0x45, 0x90
    ; log(e)
    .db 0x00, 0x7F, 0x43, 0x42, 0x94, 0x48, 0x19, 0x03, 0x25
    ; ln(10)
    .db 0x00, 0x80, 0x23, 0x02, 0x58, 0x50, 0x92, 0x99, 0x40

; (Internal) Normalize the floating point number at HL.
fpNormalize:
    push af
    push bc
    push de
        inc hl \ inc hl
        push hl
            xor a
            ld c, a
.macro fpNormalizeCountLeading
            ; Find the first nonzero byte
            or (hl)
            jr nz, _
            inc hl
            inc c
.endmacro
            fpNormalizeCountLeading
            fpNormalizeCountLeading
            fpNormalizeCountLeading
            fpNormalizeCountLeading
            fpNormalizeCountLeading
            fpNormalizeCountLeading
            fpNormalizeCountLeading
.undefine fpNormalizeCountLeading
        ; It's all zero, so clean it up and exit
        pop hl
        dec hl
        ld (hl), 0x80
        dec hl
        ld (hl), 0
        jr .end
_:
            ; HL points to the first nonzero byte
            ; Copy from HL to the beginning of the significand
            pop de \ push de
            push bc
                ld a, 7
                sub c
                ld c, a
                ld b, 0
                ldir
            pop bc
            ; Zero out the trailing bytes left over from copying
            ld b, c
            sla c
            jr z, _
            dec hl
            xor a
.zeroLoop:
            ld (hl), a
            dec hl
            djnz .zeroLoop
_:
            ; Shift out the last leading zero if necessary
            pop hl \ push hl
            ld a, (hl)
            and 0xF0
            jr nz, _
            inc c
            ld de, 6
            add hl, de
.macro fpNormalizeShiftLeft
            rld
            dec hl
.endmacro
            fpNormalizeShiftLeft
            fpNormalizeShiftLeft
            fpNormalizeShiftLeft
            fpNormalizeShiftLeft
            fpNormalizeShiftLeft
            fpNormalizeShiftLeft
            fpNormalizeShiftLeft
.undefine fpNormalizeShiftLeft
_:
        ; Fix exponent
        pop hl
        dec hl
        ld a, (hl)
        sub c
        ld (hl), a
        dec hl
.end:
    pop de
    pop bc
    pop af
    ret

;; fpAbs [FP Math]
;;  Takes the absolute value of the floating point number at IX.
;; Input:
;;  IX: Pointer to operand
;; Output:
;;  IX: Pointer to result
fpAbs:
    res 7, (ix)
    ret

;; fpNeg [FP Math]
;;  Negates the floating point number at IX.
;; Input:
;;  IX: Pointer to operand
;; Output:
;;  IX: Pointer to result
fpNeg:
    push af
        ; Make sure operand is not zero
        ld a, (ix + 2)
        or a
        jr z, .end
        ; Invert the sign bit
        ld a, (ix)
        xor 0x80
        ld (ix), a
.end:
    pop af
    ret

;; fpSub [FP Math]
;;  Subtracts the two floating point numbers.
;; Inputs:
;;  IX: Pointer to operand 1 (minuend)
;;  IY: Pointer to operand 2 (subtrahend)
;;  HL: Pointer to destination buffer
fpSub:
    push af
        ; Make sure operand is not zero
        ld a, (iy + 2)
        or a
        jr z, .end
        ; Invert the sign bit
        ld a, (iy)
        xor 0x80
        ld (iy), a
.end:
    pop af
    ; Fall through to fpAdd
;; fpAdd [FP Math]
;;  Adds the two floating point numbers.
;; Inputs:
;;  IX, IY: Pointers to operands
;;  HL: Pointer to destination buffer
;; Notes:
;;  May destroy one or both operands.
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
        ld a, (ix + 1)
        ld (hl), a
        dec hl
        ; Align smaller operand's radix point with the larger one's
        sub (iy + 1)
        jr z, .skipShifting
        cp 2
        jr c, .shiftLast
        push hl
        push de
            push af
                push iy \ pop hl
                push iy \ pop de
                ; Point DE to the end of the buffer
                ld bc, 8
                add hl, bc
                ex de, hl
                ; Point HL to the byte that will be moved to the end
                srl a       ; Number of bytes to shift right
                cp 6
                jr c, _
                ld a, 6
_:
                neg
                add a, 8
                ld c, a
                add hl, bc
                dec bc
                lddr
            ; Replace shifted digits with leading zeroes
            pop af
            ld b, 0
            ld c, a
            srl c
            add hl, bc
            ld b, c
            ld c, 0
.zeroLoop:
            ld (hl), c
            dec hl
            djnz .zeroLoop
        pop de
        pop hl
.shiftLast:
        ; Shift last digit if necessary
        and 0x01
        jr z, _
        push hl
            ; Swap with HL to make shifting easier
            push iy \ pop hl
            inc hl
            xor a
.macro fpAddShiftRight
            inc hl
            rrd
.endmacro
            fpAddShiftRight
            fpAddShiftRight
            fpAddShiftRight
            fpAddShiftRight
            fpAddShiftRight
            fpAddShiftRight
            fpAddShiftRight
.undefine fpAddShiftRight
        pop hl
.skipShifting:
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
.undefine fpAddSumIter
        ; Handle carry
        jr nc, _
        ld a, 1
        push hl
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
        pop hl
        inc (hl)
_:
    pop bc
    pop af
    pop hl
    pop iy
    pop ix
    ret

;; fpMulPow10 [FP Math]
;;  Multiplies the floating point number in IX by 10^E.
;; Input:
;;  IX: Pointer to operand
;;  E: Signed exponent (i.e. 2 -> 100, 3 -> 0.001)
;; Output:
;;  IX: Pointer to result
;; Notes:
;;  Does not check for overflow.
fpMulPow10:
    push af
        ; Make sure operand is not zero
        ld a, (ix + 2)
        or a
        jr z, .end
        ; Add the exponents
        ld a, (ix + 1)
        add e
        ld (ix + 1), a
.end:
    pop af
    ret

;; fpAnd [FP Math]
;;  Performs a logical AND on the two floating point numbers.
;; Inputs:
;;  IX, IY: Pointers to operands
;; Output:
;;  Z: Result was false
;;  NZ: Result was true
fpAnd:
    push bc
        ; Save A in B to preserve flags
        ld b, a
        ; Check if first operand is 0
        ld a, (ix + 2)
        or a
        jr z, .end
        ; Check if second operand is 0
        ld a, (iy + 2)
        or a
.end:
        ; Restore A
        ld a, b
    pop bc
    ret

;; fpOr [FP Math]
;;  Performs a logical OR on the two floating point numbers.
;; Inputs:
;;  IX, IY: Pointers to operands
;; Output:
;;  Z: Result was false
;;  NZ: Result was true
fpOr:
    push bc
        ; Save A in B to preserve flags
        ld b, a
        xor a
        ; Check if first operand is 0
        or (ix + 2)
        jr nz, .end
        ; Check if second operand is 0
        or (iy + 2)
.end:
        ; Restore A
        ld a, b
    pop bc
    ret

;; fpXor [FP Math]
;;  Performs a logical XOR on the two floating point numbers.
;; Inputs:
;;  IX, IY: Pointers to operands
;; Output:
;;  Z: Result was false
;;  NZ: Result was true
fpXor:
    push bc
        ; Save A in B to preserve flags
        ld b, a
        ; Check if first operand is 0
        ld a, (ix + 2)
        or a
        jr nz, _
        ; Check if second operand is 0
        ld a, (iy + 2)
        or a
        jr .end
_:
        ; Check if second operand is nonzero
        ld a, (iy + 2)
        or a
        jr nz, _
        or 1
        jr .end
_:
        xor a
.end:
        ; Restore A
        ld a, b
    pop bc
    ret

;; fpNot [FP Math]
;;  Performs a logical NOT on the floating point number.
;; Input:
;;  IX: Pointer to operand
;; Output:
;;  Z: Result was false
;;  NZ: Result was true
fpNot:
    push bc
        ; Save A in B to preserve flags
        ld b, a
        ; Check if operand is 0
        ld a, (ix + 2)
        or a
        jr nz, _
        or 1
        jr .end
_:
        xor a
.end:
        ; Restore A
        ld a, b
    pop bc
    ret

;; fpCompare [FP Math]
;;  Compares the two floating point numbers.
;; Inputs:
;;  IX, IY: Pointers to operands
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

;; fpMin [FP Math]
;;  Finds the minimum of the two floating point numbers.
;; Inputs:
;;  IX, IY: Pointer to operands
;; Output:
;;  HL: Pointer to minimum
fpMin:
    call fpCompare
    jr c, _
    push iy \ pop hl
    ret
_:
    push ix \ pop hl
    ret

;; fpMax [FP Math]
;;  Finds the maximum of the two floating point numbers.
;; Inputs:
;;  IX, IY: Pointers to operands
;; Output:
;;  HL: Pointer to maximum
fpMax:
    call fpCompare
    jr c, _
    push ix \ pop hl
    ret
_:
    push iy \ pop hl
    ret

;; fpRand [FP Math]
;;  Generates a random floating point number between 0 and 1, similar to
;;  TI-OS's `rand` command.
;; Input:
;;  HL: Pointer to output
;; Notes:
;;  Uses `getRandom` to generate the digits, so it is not cryptographically
;;  random.
fpRand:
    push af
    push bc
    push de
        push hl
            ld (hl), 0
            inc hl
            ld (hl), 0x7F
            inc hl
.macro fpRandIter
            call getRandom
            ld b, a
            and 0xF0
            ld d, a
            call div8By8
            rld
            ld a, b
            and 0x0F
            ld d, a
            call div8By8
            rld
            inc hl
.endmacro
            ld e, 10
            fpRandIter
            fpRandIter
            fpRandIter
            fpRandIter
            fpRandIter
            fpRandIter
            fpRandIter
        pop hl
        ; There may be leading zeroes, so normalize.
        ; Note: There is no need to add extra random digits to compensate for
        ; removed leading zeroes. Even leading zeroes count towards entropy,
        ; so every result output from this function should have exactly
        ; 14 digits of entropy (roughly 46.5 bits).
        call fpNormalize
.undefine fpRandIter
    pop de
    pop bc
    pop af
    ret

;; fpIPart [FP Math]
;;  Calculates the integer part of a floating point number, similar to
;;  TI-OS's `iPart()` command.
;; Input:
;;  IX: Pointer to operand
;; Output:
;;  IX: Pointer to result
fpIPart:
    push af
    push bc
    push hl
        push ix \ pop hl
        inc hl \ inc hl
        ; Check exponent
        ld a, (ix + 1)
        ; Very large numbers don't store a fractional part, so skip them
        cp 0x80 + 14
        jr nc, .end
        ; Negative exponents have no integer part
        cp 0x80
        jr nc, _
        ld (ix + 1), 0x80
        xor a
        jr .beginZeroLoop
_:
        sub 0x7F
        ; Point HL to the first fractional byte
        ld b, 0
        ld c, a
        srl c
        add hl, bc
.beginZeroLoop:
        ; If there are an odd number of integer digits, only zero half of
        ; the first byte
        ld b, a
        and 1
        jr z, _
        ld a, (hl)
        and 0xF0
        ld (hl), a
        inc hl
        inc b
_:
        ld a, 14
        sub b
        ld b, a
        srl b
        jr z, .end
.zeroLoop:
        ld (hl), 0
        inc hl
        djnz .zeroLoop
.end:
    pop hl
    pop bc
    pop af
    ret

;; fpFPart [FP Math]
;;  Calculates the fractional part of a floating point number, similar to
;;  TI-OS's `fPart()` command.
;; Input:
;;  IX: Pointer to operand
;; Output:
;;  IX: Pointer to result
fpFPart:
    push af
    push bc
    push hl
        push ix \ pop hl
        inc hl \ inc hl
        ; Check exponent
        ld a, (ix + 1)
        ; Negative exponents are entirely fractional, so skip them
        cp 0x80
        jr c, .end
        ; Very large numbers have no fractional part
        cp 0x80 + 14
        jr c, _
        ld (ix + 1), 0x80
        xor a
        jr .beginZeroLoop
_:
        sub 0x80
        ; Point HL to the last integer byte
        ld b, 0
        ld c, a
        srl c
        add hl, bc
.beginZeroLoop:
        ; If there are an odd number of integer digits, only zero half of
        ; the first byte
        ld b, a
        and 1
        jr nz, _
        ld a, (hl)
        and 0x0F
        ld (hl), a
        dec hl
        dec b
_:
        inc b
        srl b
        jr z, .normalize
.zeroLoop:
        ld (hl), 0
        dec hl
        djnz .zeroLoop
.normalize:
        push ix \ pop hl
        call fpNormalize
.end:
    pop hl
    pop bc
    pop af
    ret
