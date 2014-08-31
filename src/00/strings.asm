;; stringLength [Strings]
;;  Determines the length of a zero delimited string.
;; Inputs:
;;  HL: String pointer
;; Outputs:
;;  BC: String length
stringLength:
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
    
;; compareStrings [Strings]
;;  Determines if two strings are equal, and checks alphabetical sort order.
;; Inputs:
;;  HL: String pointer
;;  DE: String pointer
;; Outputs:
;;  Z: Set if equal, reset if not equal
;;  C: Set if string HL is alphabetically earlier than string DE
compareStrings:
    ld a, (de)
    or a
    jr z, .end
    cp (hl)
    jr nz, .exit
    inc hl
    inc de
    jr compareStrings
.end:
    ld a, (hl)
    or a
.exit:
    ccf
    ret
    
;; compareStrings_sort [Strings]
;;  Compares strings at ((HL)) and ((DE)).  That is, calls indirect16HLDE,
;;  then calls compareStrings.
;; Inputs:
;;  HL: Pointer to string pointer
;;  DE: Pointer to string pointer
;; Outputs:
;;  Z: Set if equal, reset if not equal
;;  C: Set if string (HL) is alphabetically earlier than string (DE)
;; Notes:
;;  This routine is extremely useful as the callback for the [[callbackSort]] routine.
;;  It allows sorting a list of pointers to strings by the strings' sort order.
compareStrings_sort:
    push hl
    push de
        call indirect16HLDE
        call compareStrings
_:  pop de
    pop hl
    ret
    
;; stringCopy [Strings]
;;  Copies a string.
;; Inputs:
;;  HL: String pointer
;;  DE: Destination
stringCopy:
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
;;  Converts an ASCII-encoded unsigned decimal into a word.
;; Inputs:
;;  HL: pointer on ASCII-encoded decimal
;; Outputs:
;;  DE: converted byte
;;  Z: set on success
;; Notes:
;;  The routine will scan 5-digits numbers that are over 65535 
;; as valid numbers, even if HL cannot contain said numbers.
strtoi:
    push hl \ push bc
        push hl
            ld b, 0
.countDigits:
            ld a, (hl)
            cp '0'
            jr c, .noMoreDigits
            cp '9' + 1
            jr nc, .noMoreDigits
            inc hl
            inc b
            ld a, 4
            cp b
            jr nc, .countDigits
.noMoreDigits:
            xor a
            cp b
        pop hl
        jr z, .error
        ; HL = string
        ; B = number of digits
        ; for(; B > 0 ; HL++, B--)
        ;   num += (*HL - '0') * miniLUT[B - 1];
        ld c, 0
        push hl
            ld hl, 0
            ex (sp), hl
.formByteLoop:
            ld a, (hl)
            sub '0'
            ex (sp), hl
            push hl
                ld hl, .factorsLUT
                ld e, b
                ld d, 0
                dec e
                add hl, de
                add hl, de
                ld e, (hl)
                inc hl
                ld d, (hl)
                call mul16By8
            pop de
            add hl, de
            ex (sp), hl
            inc hl
            djnz .formByteLoop
        pop de
        xor a
        jr .error + 2
.error:
        xor a
        inc a
    pop bc \ pop hl
    ret
.factorsLUT:
    .dw 1, 10, 100, 1000, 10000
