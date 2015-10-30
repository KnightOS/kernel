;; draw3x3Char [Text]
;;  Draws a character to the screen buffer with the 3x3 font.
;; Inputs:
;;  IY: Screen buffer
;;  A: Character to print
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Moved to next character position
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
draw3x3Char:
    push af
    push hl
    push bc
        cp '\n'
        jr nz, _
        ld a, e
        add a, 6
        ld e, a
        ld d, b
        jr .exit
_:
        cp '\r'
        jr nz, _
        ld d, b
        jr .exit
_:
        cp '\t'
        jr nz, _
        ld a, d
        add a, 8
        ld d, a
        jr .exit
_:
        push de
            sub 0x20
            ld l, a
            ld h, 0
            ld d, h \ ld e, l
            add hl, de \ add hl, de
            ex de, hl
            ld hl, font_3x3
            add hl, de
        pop de
        ld b, 3
        push af
            ld a, d
            cp 95
            jr nc, +_
            call putSpriteOR
        pop af
        ld a, 4
        add a, d
        ld d, a
.exit:
    pop bc
    pop hl
    pop af
    ret
_:      pop af
    pop bc
    pop hl
    pop af
    ret

;; draw3x3Str [Text]
;;  Draws a zero-delimited string to the screen buffer using the 3x3 font.
;; Inputs:
;;  IY: Screen buffer
;;  HL: String
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Advanced to position of the end of the string
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
draw3x3Str:
    push hl
    push af
_:      ld a, (hl)
        or a
        jr z, _
        call draw3x3Char
        inc hl
        jr -_
_:  pop af
    pop hl
    ret

;; draw3x3HexA [Text]
;;  Draws the contents of A in hexadecimal to the screen buffer using the 3x3 font.
;; Inputs:
;;  IY: Screen buffer
;;  D, E: X, Y
;;  A: Value
;; Outputs:
;;  D, E: Advanced to position of next character
draw3x3HexA:
    push af
        rrca
        rrca
        rrca
        rrca
        call .dispha
    pop af
    ; fall into for second call
.dispha:
    and 15
    cp 10
    jr nc, .dhlet
    add a, 48
    jr .dispdh
.dhlet:
    add a, 55
.dispdh:
    jp draw3x3Char

;; draw3x3DecA [Text]
;;  Draws the contents of A in decimal to the screen buffer using the 3x3 font.
;; Inputs:
;;  IY: Screen buffer
;;  D, E: X, Y
;;  A: Value
;; Outputs:
;;  D, E: Advanced to position of next character
draw3x3DecA:
    push af
    push bc
    push hl
        ; use C to keep track of leading zeroes
        ld c, 0
        ; display hundreds
        push af
            push de
                ld e, 100
                ld d, a
                call div8By8
                ld a, d
            pop de
            or a
            jr z, .no100
            inc c
            ld b, a
            add a, '0'
            call draw3x3Char
            ld a, b
            ld b, e
            ld e, a
            ld h, 100
            call mul8By8
            ld e, b
        pop af
        sub l
        jr .done100
.no100:
        pop af
.done100:
        ; display tens
        push af
            push de
                ld e, 10
                ld d, a
                call div8By8
                ld a, d
            pop de
            ld b, a
            or a
            ; this may not be a leading zero
            or c
            ld a, b
            jr z, .no10
            ld b, a
            add a, '0'
            call draw3x3Char
            ld a, b
            ld b, e
            ld e, a
            ld h, 10
            call mul8By8
            ld e, b
        pop af
        sub l
        jr .done10
.no10:
        pop af
.done10:
        ; draw units
        add a, '0'
        call draw3x3Char
    pop hl
    pop bc
    pop af
    ret
    
;; draw3x3HexHL [Text]
;;  Draws the contents of HL in hexadecimal to the screen buffer using the small font.
;; Inputs:
;;  IY: Screen buffer
;;  D, E: X, Y
;;  HL: Value
;; Outputs:
;;  D, E: Advanced to position of next character
draw3x3HexHL:
    push af
        ld a, h
        call draw3x3HexA
        ld a, l
        call draw3x3HexA
    pop af
    ret
   
;; draw3x3DecHL [Text]
;;  Draws the contents of HL in decimal to the screen buffer using the small font.
;; Inputs:
;;  IY: Screen buffer
;;  D, E: X, Y
;;  HL: Value
;; Outputs:
;;  D, E: Advanced to position of next character
draw3x3DecHL:
    push hl
    push bc
    push af
        ld b, 0           ;Our digit counter
.loop:
        push de
            ld de, 0
            call cpHLDE
        pop de
        jr z, _           ;If HL is down to zero, exit loop
        ld c, 10
        call divHLByC     ;Divide HL by 10...
        push af           ;Push the remainder to the stack
        inc b             ;Inc our digit counter
        jr .loop
_:
        ld a, b
        cp 0
        call z, draw3x3DecA  ;Draw a Zero if HL is Zero
.draw:
        ld a, b
        cp 0
        jr z, _           ;if our digit counter is zero, exit loop
        pop af            ;pop our digit from the stack
        call draw3x3DecA     ;draw the digit
        dec b             ;dec our digit counter
        jr .draw
_:
    pop af
    pop bc
    pop hl
    ret

;; draw3x3DecACIX [Text]
;;  Draws the contents of ACIX in decimal to the screen buffer using the 3x3 font.
;; Inputs:
;;  IY: Screen buffer
;;  D, E: X, Y
;;  ACIX: Value
;; Outputs:
;;  D, E: Advanced to position of next character
draw3x3DecACIX:
    push af
    push ix
    push bc
    push hl
        ld b, 0           ;Our digit counter
.loop:
        ;Check if ACIX is zero
        push bc
            cp 0
            jp nz, .notZero     ;If a is not zero

            ld b, a
            ld a, c
            cp 0
            ld a, b
            jp nz, .notZero     ;If c is not zero

            ld b, a
            ld a, ixh
            cp 0
            ld a, b
            jp nz, .notZero     ;If ixh is not zero

            ld b, a
            ld a, ixl
            cp 0
            ld a, b
            jp nz, .notZero     ;If ixl is not zero
.notZero:
        pop bc
        jr z, _           ;If ACIX is down to zero, exit loop
        push de
        push iy
            ld iyh, b
            ld de, 10
            call div32By16     ;Divide ACIX by 10...
            ld b, iyh
        pop iy
        pop de
        push hl           ;Push the remainder to the stack
        inc b             ;Inc our digit counter
        jr .loop
_:
        ld a, b
        cp 0
        call z, drawDecA  ;Draw a Zero if ACIX is Zero
.draw:
        ld a, b
        cp 0
        jr z, _           ;if our digit counter is zero, exit loop
        pop hl            ;pop our digit from the stack
        call drawDecHL    ;draw the digit
        dec b             ;dec our digit counter
        jr .draw
_:
    pop hl
    pop bc
    pop ix
    pop af
    ret
