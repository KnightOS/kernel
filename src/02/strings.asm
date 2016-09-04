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
