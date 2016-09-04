;; strlen [Strings]
;;  Determines the length of a zero delimited string.
;; Inputs:
;;  HL: String pointer
;; Outputs:
;;  BC: String length
strlen:
    push af
    push hl
        xor a
        ld b, a
        ld c, a
        cpir
        ; bc = -bc
        xor a \ sub c \ ld c, a \ sbc a, a \ sub b \ ld b, a
        dec bc
    pop hl
    pop af
    ret
    
;; strcmp [Strings]
;;  Determines if two strings are equal, and checks alphabetical sort order.
;; Inputs:
;;  HL: String pointer
;;  DE: String pointer
;; Outputs:
;;  Z: Set if equal, reset if not equal
;;  C: Set if string HL is alphabetically earlier than string DE
strcmp:
    push hl
    push de
.loop:
        ld a, (de)
        or a
        jr z, .end
        cp (hl)
        jr nz, .exit
        inc hl
        inc de
        jr .loop
.end:
        ld a, (hl)
        or a
.exit:
        ccf
    pop de
    pop hl
    ret
    
;; strcmp_sort [Strings]
;;  Compares strings at ((HL)) and ((DE)).  That is, calls [[indirect16HLDE]],
;;  then calls [[strcmp]].
;; Inputs:
;;  HL: Pointer to string pointer
;;  DE: Pointer to string pointer
;; Outputs:
;;  Z: Set if equal, reset if not equal
;;  C: Set if string (HL) is alphabetically earlier than string (DE)
;; Notes:
;;  This routine is useful as the callback for the [[callbackSort]] routine.
;;  It allows sorting a list of pointers to strings in alphabetical order.
strcmp_sort:
    push hl
    push de
        call indirect16HLDE
        call strcmp
    pop de
    pop hl
    ret
    
;; strcpy [Strings]
;;  Copies a string.
;; Inputs:
;;  HL: String pointer
;;  DE: Destination
;; Notes:
;;  This will trample into undefined territory if you try to copy a string into some
;;  allocated memory it won't fit in.
strcpy:
    push de
    push hl
    ex de, hl
_:  ld a, (de)
    ld (hl), a
    or a
    jr z, _
    inc hl \ inc de
    jr -_
_:  pop hl
    pop de
    ret
    
;; strchr [Strings]
;;  Returns a pointer on the first occurence of a character in a string.
;; Inputs:
;;  HL: Haystack
;;  B: Needle
;; Outputs:
;;  HL: Pointer to first occurence of character
;;  Z: Set if character found
;;  A: Destroyed
strchr:
.loop:
    ld a, (hl)
    or a
    jr z, .noCharFound
    cp b
    ret z
    inc hl
    jr .loop
.noCharFound:
    inc a
    ret
    
;; strtoi [Strings]
;;  Converts an ASCII-encoded signed decimal into a number.
;; Inputs:
;;  HL: Pointer on ASCII-encoded decimal
;;  B: Maximum number of digits to convert
;; Outputs:
;;  DEHL: Converted word
;;  Z: Set on success, reset on error
;; Notes:
;;  This will ignore leading zeros and has an unsigned range of 0 to 4,294,967,295.
;;  Signed numbers may be prefixed with '-' and have a range of -2,147,483,648 to
;;  2,147,483,647. Strings whose numbers fall outside that range produce undefined
;;  behavior.
strtoi:
    push bc
        ld a, (hl)
        sub '-'
        jr nz, $+3
        inc hl
        push af
.skipLeadingLoop:
            ld a, (hl)
            cp '0'
            jr nz, .noMoreLeading
            inc hl
            ld a, (hl)
            cp '0'
            jr c, .return0
            cp '9' + 1
            jr c, .skipLeadingLoop
.return0:
            ld hl, 0
            ld de, 0
        pop af
        xor a
    pop bc
    ret
.noMoreLeading:
            push hl
                dec b
                ld c, 0
.countDigits:
                ld a, (hl)
                cp '0'
                jr c, .noMoreDigits
                cp '9' + 1
                jr nc, .noMoreDigits
                inc hl
                inc c
                ld a, 9 ; maximum of 10 digits
                cp c
                jr c, .noMoreDigits
                ld a, b
                cp c
                jr nc, .countDigits
.noMoreDigits:
                xor a
                cp c
            pop hl
            jr z, .error
            ; HL = string
            ; B = number of digits
            ; for(; B >= 0 ; string++, B--)
            ;   DEHL += (*string - '0') * miniLUT[B - 1];
            exx
            ld de, 0
            ld hl, 0
            exx
            ld b, c
.formWordLoop:
            ld a, (hl)
            sub '0'
            push hl
                ld l, b
                ld h, 0
                dec l
                add hl, hl
                add hl, hl
                ld de, .factorsLUT + 3
                add hl, de
                ld d, (hl)
                dec hl
                ld e, (hl)
                dec hl
                ld c, (hl)
                dec hl
                ld l, (hl)
                ld h, c
                call mul32By8
                push de
                    push hl
                        exx
                    pop bc
                    add hl, bc
                    ex de, hl
                pop bc
                adc hl, bc
                ex de, hl
                exx
            pop hl
            inc hl
            djnz .formWordLoop
            exx
            push de \ push hl
                exx
            pop hl \ pop de
        pop af
        jr nz, .noNegate
        ld b, h
        ld c, l
        ld hl, 0
        or a
        sbc hl, bc
        ld b, h
        ld c, l
        ld hl, 0
        sbc hl, de
        ld d, h
        ld e, l
        ld h, b
        ld l, c
.noNegate:
        xor a
        jr .error + 3
.error:
        pop af
        xor a
        inc a
    pop bc
    ret
    
    ; 4-bytes factors
.factorsLUT:
    .dw 1, 0
    .dw 10, 0
    .dw 100, 0
    .dw 1000, 0
    .dw 10000, 0
    .dw 0x86a0, 0x0001    ;       100,000
    .dw 0x4240, 0x000f    ;     1,000,000
    .dw 0x9680, 0x0098    ;    10,000,000
    .dw 0xe100, 0x05f5    ;   100,000,000
    .dw 0xca00, 0x3b9a    ; 1,000,000,000

;; itostr [Strings]
;;  Converts an 32 bit unsigned integer into an ASCII-encoded decimal string.
;; Inputs:
;;  ACIX: Number
;;  HL: String pointer
;; Notes:
;;  This will not have leading zeros and has a range of 0 to 4,294,967,295.
itostr:
    push af
    push ix
    push bc
    push hl
    push iy
        push hl \ pop iy
        ld b, 0     ;Our digit counter
.loop:
        ;Check if ACIX is zero
        push bc
            ;If a is not zero
            cp 0
            jp nz, .notZero
            ;If c is not zero
            ld b, a
            ld a, c
            cp 0
            ld a, b
            jp nz, .notZero
            ;If ixh is not zero
            ld b, a
            ld a, ixh
            cp 0
            ld a, b
            jp nz, .notZero
            ;If ixl is not zero
            ld b, a
            ld a, ixl
            cp 0
            ld a, b
.notZero:
        pop bc
        ;If ACIX is down to zero, exit loop
        jr z, _
        push de
        ;Divide ACIX by 10...
        push iy
            ld iyh, b
            ld de, 10
            call div32By16
            ld b, iyh
        pop iy
        pop de
        push hl     ;Push the remainder to the stack
        inc b       ;Inc our digit counter
        jr .loop
_:
        ; Write a Zero if ACIX is Zero
        ld a, b
        cp 0
        jr nz, .write
        ld (iy), '0'
        inc iy
.write:
        ld a, b
        cp 0
        jr z, _     ;If our digit counter is zero, exit loop
        pop hl      ;pop our digit from the stack
        ;write the digit
        ld a, '0'
        add a, l
        ld (iy), a
        inc iy
        dec b       ;dec our digit counter
        jr .write
_:
        ld (iy), 0  ;End string
    pop iy
    pop hl
    pop bc
    pop ix
    pop af
    ret
    
;; toLower [Strings]
;;  Converts every alpha character of a string to lowercase.
;; Inputs:
;;  HL: Pointer to string
;; Notes:
;;  This modifies the string in-place.
toLower:
    push af \ push hl
        ld a, (hl)
        or a
        jr z, .exit
        call isAlphaNum
        jr nc, .notUpperAlpha
        cp 'a'
        jr nc, .notUpperAlpha
        add a, 'a' - 'A'
        ld (hl), a
.notUpperAlpha:
        inc hl
        jr toLower + 2
.exit:
    pop hl \ pop af
    ret
    
;; toUpper [Strings]
;;  Converts every alpha character of a string to uppercase.
;; Inputs:
;;  HL: Pointer to string
;; Notes:
;;  This modifies the string in-place.
toUpper:
    push af \ push hl
        ld a, (hl)
        or a
        jr z, .exit
        call isAlphaNum
        jr nc, .notLowerAlpha
        cp 'a'
        jr c, .notLowerAlpha
        sub 'a' - 'A'
        ld (hl), a
.notLowerAlpha:
        inc hl
        jr toUpper + 2
.exit:
    pop hl \ pop af
    ret
