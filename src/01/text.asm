; Text functions for kernel

;; newline [Text]
;;  Advances D, E to the next line of text
;; Inputs:
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Moved down one line and to the left margin
;; Notes:
;;  This is identical to (but faster than) calling drawChar with '\n'
newline:
    push af
        ld a, e
        add a, 6
        ld e, a
        ld d, b
    pop af
    ret

;; drawChar [Text]
;;  Draws a character to the screen buffer using OR logic (turns pixels ON).
;; Inputs:
;;  IY: Screen buffer
;;  A: Character to print
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Moved to next character position
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
drawChar:
    push ix
        ld ixl, 0
        call drawCharShared
    pop ix
    ret

;; drawCharAND [Text]
;;  Draws a character to the screen buffer using AND logic (turns pixels OFF).
;; Inputs:
;;  IY: Screen buffer
;;  A: Character to print
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Moved to next character position
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
drawCharAND:
    push ix
        ld ixl, 1
        call drawCharShared
    pop ix
    ret

;; drawCharXOR [Text]
;;  Draws a character to the screen buffer using XOR logic (inverts pixels).
;; Inputs:
;;  IY: Screen buffer
;;  A: Character to print
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Moved to next character position
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
drawCharXOR:
    push ix
        ld ixl, 2
        call drawCharShared
    pop ix
    ret
    
drawCharShared:
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
        add a, 4
        ld d, a
        jr .exit
_:
        push de
            sub 0x20
            ld l, a
            ld h, 0
            add hl, hl
            ld d, h
            ld e, l
            add hl, hl
            add hl, de
            ex de, hl
            ld hl, kernel_font
            add hl, de
            ld a, (hl)
            inc hl
        pop de
        
        ld b, 5
        push af
            ld a, d
            cp 95
            jr nc, +_
            ld a, ixl
            or a
            call z, putSpriteOR
            dec a
            call z, putSpriteAND
            dec a
            call z, putSpriteXOR
        pop af
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

;; drawStr [Text]
;;  Draws a zero-delimited string to the screen buffer using OR logic (turns pixels ON).
;; Inputs:
;;  IY: Screen buffer
;;  HL: String
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Advanced to position of the end of the string
;;  HL: Advanced to the null terminator
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
drawStr:
    push ix
        ld ixl, 0
        call drawStrShared
    pop ix
    ret

;; drawStrAND [Text]
;;  Draws a zero-delimited string to the screen buffer using AND logic (turns pixels OFF).
;; Inputs:
;;  IY: Screen buffer
;;  HL: String
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Advanced to position of the end of the string
;;  HL: Advanced to the null terminator
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
drawStrAND:
    push ix
        ld ixl, 1
        call drawStrShared
    pop ix
    ret

;; drawStrXOR [Text]
;;  Draws a zero-delimited string to the screen buffer using XOR logic (inverts pixels).
;; Inputs:
;;  IY: Screen buffer
;;  HL: String
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Advanced to position of the end of the string
;;  HL: Advanced to the null terminator
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
drawStrXOR:
    push ix
        ld ixl, 2
        call drawStrShared
    pop ix
    ret
    
drawStrShared:
    push af
_:      ld a, (hl)
        or a
        jr z, _
        call drawCharShared
        inc hl
        jr -_
_:  pop af
    ret
    
;; wrapStr [Text]
;;  Draws a zero-delimited string to the screen buffer using OR logic (turns pixels ON),
;;  and wraps it around the screen with character breaks.
;; Inputs:
;;  IY: Screen buffer
;;  HL: String
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Advanced to position of the end of the string
;;  HL: Advanced to the null terminator or to the next character that would have been drawn if the string hadn't run off-screen.
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
wrapStr:
    push ix
        ld ixl, 0
        call wrapStrShared
    pop ix
    ret

;; wrapStrAND [Text]
;;  Draws a zero-delimited string to the screen buffer using AND logic (turns pixels OFF),
;;  and wraps it around the screen with character breaks.
;; Inputs:
;;  IY: Screen buffer
;;  HL: String
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Advanced to position of the end of the string
;;  HL: Advanced to the null terminator or to the next character that would have been drawn if the string hadn't run off-screen.
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
wrapStrAND:
    push ix
        ld ixl, 1
        call wrapStrShared
    pop ix
    ret

;; wrapStrXOR [Text]
;;  Draws a zero-delimited string to the screen buffer using XOR logic (inverts pixels),
;;  and wraps it around the screen with character breaks.
;; Inputs:
;;  IY: Screen buffer
;;  HL: String
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Advanced to position of the end of the string
;;  HL: Advanced to the null terminator or to the next character that would have been drawn if the string hadn't run off-screen.
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
wrapStrXOR:
    push ix
        ld ixl, 2
        call wrapStrShared
    pop ix
    ret

wrapStrShared:
    push af
_:      ld a, (hl)
        or a
        jr z, _
        push af \ push hl
            push de
                sub 0x20
                ld l, a
                ld h, 0
                add hl, hl
                ld d, h
                ld e, l
                add hl, hl
                add hl, de
                ex de, hl
                ld hl, kernel_font
                add hl, de
            pop de
            ld a, (hl)
            inc hl
            add a, d
            cp MONO_LCD_WIDTH
            call nc, newline
        pop hl \ pop af
        call drawCharShared
        ld a, e
        cp MONO_LCD_HEIGHT
        jr nc, _
        inc hl
        jr -_
_:  pop af
    ret

;; drawHexA [Text]
;;  Draws the contents of A in hexadecimal to the screen buffer using OR logic (turns pixels ON).
;; Inputs:
;;  IY: Screen buffer
;;  D, E: X, Y
;;  A: Value
;; Outputs:
;;  D, E: Advanced to position of next character
drawHexA:
    push af
        rrca
        rrca
        rrca
        rrca
        call dispha
    pop af
    ; fall into for second call
dispha:
    and 15
    cp 10
    jr nc, dhlet
    add a, 48
    jr dispdh
dhlet:
    add a, 55
dispdh:
    jp drawChar

;; drawHexHL [Text]
;;  Draws the contents of HL in hexadecimal to the screen buffer using OR logic (turns pixels ON).
;; Inputs:
;;  IY: Screen buffer
;;  D, E: X, Y
;;  HL: Value
;; Outputs:
;;  D, E: Advanced to position of next character
drawHexHL:
    push af
        ld a, h
        call drawHexA
        ld a, l
        call drawHexA
    pop af
    ret
   
;; measureChar [Text]
;;  Measures the width of a character in pixels.
;; Inputs:
;;  A: Character to measure
;; Outputs:
;;  A: Width of character
;; Notes:
;;  The height of each character is always 5 pixels. The width also often includes a column of empty pixels on the right (exceptions include '_').
measureChar:
    push hl
    push de
    push af
        ld de, 6
        sub 0x20
        call mul16By8
        ex de, hl
        ld hl, kernel_font
        add hl, de
        ld a, (hl)
    pop af
    pop de
    pop hl
    ret

;; measureStr [Text]
;;  Measures the width of a string in pixels.
;; Inputs:
;;  HL: String to measure
;; Outputs:
;;  A: Width of string
;; Notes:
;;  The height of any string is always 5 pixels. This function does not support newlines.
measureStr:
    push hl
    push bc
_:     push af
            ld a, (hl)
            or a
            jr z, _
            call measureChar
        pop bc
        add a, b
        ld a, b
        inc hl
        jr -_
_:  pop af
    pop bc
    pop hl
    ret
