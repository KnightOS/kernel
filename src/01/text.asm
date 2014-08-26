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

wrapChar:
    push ix
        ld ixl, 0
        call wrapCharShared
    pop ix
    ret

wrapCharAND:
    push ix
        ld ixl, 1
        call wrapCharShared
    pop ix
    ret

wrapCharXOR:
    push ix
        ld ixl, 2
        call wrapCharShared
    pop ix
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
        add a, 6
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

; Inputs:
;  A:   Char
;  IXH: Margin
;  IXL: Draw using OR, AND, or XOR
;  D,E: Drawing coordinates
;  B,C: Bounding box limits
wrapCharShared:
    push af
    push bc
    push hl

        ; If char is a newline, move to beggining of next line
        cp '\n'
        jr nz, _
        ld a, e
        add a, 6
        ld e, a         ; Move down 6 pixels (1 row of text)
        ld d, ixh       ; Move to left margin
        jr .exit

_:      ; If char is carriage return, move to beggining of line
        ; What the hell is the point of a carriage return anyways?
        cp '\r'
        jr nz, _
        ld d, ixh
        jr .exit

_:      ; If char is a tab, move right 6 pixels
        ; guess we don't do tab alignment
        cp '\t'
        jr nz, _
        ld a, d
        add a, 6
        ld d, a
        jr .exit

_:      ; Get index of char in kernel font table
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
            ld a, (hl)          ; Load width of char into A
            inc hl              ; Load pointer to sprite in HL
        pop de

        ; Check for wrapping
        ; If right side of char goes beyond limit...
        add a, d
        cp b
        jr c, _
        ; ...shift down 6 pixels and over to left margin
        ld a, e
        add a, 6
        ld e, a
        ld d, ixh

        ; If Y coordinate is not below screen...
_:      ld a, e
        cp c
        jr nc, _
        ; ...draw sprite
        ld b, 5         ; set sprite height
        ld a, ixl
        or a            ; C=0 for OR
        call z, putSpriteOR
        dec a           ; C=1 for AND
        call z, putSpriteAND
        dec a           ; C=2 for XOR
        call z, putSpriteXOR
        dec hl
        ld a, (hl)      ; Load width of char into a
        add a, d
        ld d, a         ; add width of char to X coordinate
.exit:
    pop hl
    pop bc
    pop af
    ret
_:
    ld e, c         ; Set Y to lower limit
    jr .exit

;; drawStr [Text]
;;  Draws a zero-delimited string to the screen buffer using OR logic (turns pixels ON).
;; Inputs:
;;  IY: Screen buffer
;;  HL: String
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Advanced to position of the end of the string
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
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
drawStrXOR:
    push ix
        ld ixl, 2
        call drawStrShared
    pop ix
    ret
    
drawStrShared:
    push hl
    push af
_:      ld a, (hl)
        or a
        jr z, _
        call drawCharShared
        inc hl
        jr -_
_:  pop af
    pop hl
    ret

;; wrapStr [Text]
;;  Draws a zero-delimited string to the screen buffer using OR logic (turns pixels ON),
;;  and wraps it inside a rectangle, with character breaks.
;; Inputs:
;;  HL: String pointer
;;  IY: Screen buffer
;;  A: Left margin
;;  D, E: X, Y
;;  B, C: X Limit, Y Limit
;; Outputs:
;;  D, E: Advanced to position of the end of the string
;;  HL: Pointer to null terminator or next character that would have been drawn if the string hadn't run off-screen.
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
wrapStr:
    push ix
        ld ixh, a
        ld ixl, 0       ; Tell wrapCharShared to use OR logic
        call wrapStrShared
    pop ix
    ret

;; wrapStrAND [Text]
;;  Draws a zero-delimited string to the screen buffer using AND logic (turns pixels OFF),
;;  and wraps it inside a rectangle, with character breaks.
;; Inputs:
;;  HL: String pointer
;;  IY: Screen buffer
;;  A: Left margin
;;  D, E: X, Y
;;  B, C: X Limit, Y Limit
;; Outputs:
;;  D, E: Advanced to position of the end of the string
;;  HL: Pointer to null terminator or next character that would have been drawn if the string hadn't run off-screen.
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
wrapStrAND:
    push ix
        ld ixh, a
        ld ixl, 1       ; Tell wrapCharShared to use AND logic
        call wrapStrShared
    pop ix
    ret

;; wrapStrXOR [Text]
;;  Draws a zero-delimited string to the screen buffer using XOR logic (inverts pixels),
;;  and wraps it inside a rectangle, with character breaks.
;; Inputs:
;;  HL: String pointer
;;  IY: Screen buffer
;;  A: Left margin
;;  D, E: X, Y
;;  B, C: X Limit, Y Limit
;; Outputs:
;;  D, E: Advanced to position of the end of the string
;;  HL: Pointer to null terminator or next character that would have been drawn if the string hadn't run off-screen.
;; Notes:
;;  The left margin is only required if your string contains newlines or carriage returns.
wrapStrXOR:
    push ix
        ld ixh, a
        ld ixl, 2       ; Tell wrapCharShared to use XOR logic
        call wrapStrShared
    pop ix
    ret
    
wrapStrShared:
    push af
        ; Return if next char is null
_:      ld a, (hl)
        or a
        jr z, _

        call wrapCharShared
        ; Return if Y coordinate is greater than lower limit
        ld a, e
        cp c
        jr nc, _

        ; Go to next char
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
        ld de, 6
        sub 0x20
	jr c, _		; Return 0 if char < 0x20
        call mul16By8
        ex de, hl
        ld hl, kernel_font
        add hl, de
        ld a, (hl)
.exit:
    pop de
    pop hl
    ret

_:  xor a
    jr .exit

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
        ld b, 0
.loop:
        ld a, (hl)
        or a
        jr z, _
        call measureChar
        add a, b
        ld b, a
        inc hl
        jr .loop
_:  ld a, b
    pop bc
    pop hl
    ret
