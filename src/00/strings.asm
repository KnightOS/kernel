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
    ld a, (de)
    or a
    jr z, .end
    cp (hl)
    jr nz, .exit
    inc hl
    inc de
    jr strcmp
.end:
    ld a, (hl)
    or a
.exit:
    ccf
    ret
    
;; strcmp_sort [Strings]
;;  Compares strings at ((HL)) and ((DE)).  That is, calls indirect16HLDE,
;;  then calls strcmp.
;; Inputs:
;;  HL: Pointer to string pointer
;;  DE: Pointer to string pointer
;; Outputs:
;;  Z: Set if equal, reset if not equal
;;  C: Set if string (HL) is alphabetically earlier than string (DE)
;; Notes:
;;  This routine is extremely useful as the callback for the [[callbackSort]] routine.
;;  It allows sorting a list of pointers to strings by the strings' sort order.
strcmp_sort:
    push hl
    push de
        call indirect16HLDE
        call strcmp
_:  pop de
    pop hl
    ret
    
;; strcpy [Strings]
;;  Copies a string.
;; Inputs:
;;  HL: String pointer
;;  DE: Destination
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
_:  pop de
    pop hl
    ret
    
;; strchr [Strings]
;;  Returns a pointer on the first occurence of a character in a string.
;; Inputs:
;;  HL: string pointer
;;  B: character to search
;; Outputs:
;;  HL: pointer on first occurence of character in string in case of success
;;  Z: set if character found
;; Notes:
;;  Destroys A
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
;;  Converts an ASCII-encoded unsigned decimal into a word of variable size.
;; Inputs:
;;  HL: pointer on ASCII-encoded decimal
;;  B: maximum number of digits to convert
;; Outputs:
;;  DEHL: converted word
;;  Z: set on success, reset on error
;; Notes:
;;  The routine will ignore leading zeroes to produce a number composed by a 
;;  maximum of 10 digits, the maximal value being 4,294,967,295. If a 10-digits
;;  number with a greater value is encountered, no error will be thrown but the
;;  number won't be converted as expected.
;;  Besides, the routine supports negative numbers that starts with the character '-'.
;;  This character has no effect on the number of digits parsed.
;;  Destroys BC', DE' and HL'.
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
            jr .skipLeadingLoop
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
    .dw 0x86a0, 0x0001    ; 100,000
    .dw 0x4240, 0x000f    ; 1,000,000
    .dw 0x9680, 0x0098    ; 10,000,000
    .dw 0xe100, 0x05f5    ; 100,000,000
    .dw 0xca00, 0x3b9a    ; 1,000,000,000
