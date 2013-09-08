; Text functions for kernel

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
;;  The left margin is only required if your string contains newlines.
drawChar:
    push af
    push hl
    push ix
    push bc
        cp '\n'
        jr nz, _
        ld a, e
        add a, 6
        ld e, a
        ld d, b
        jr ++_
    
_:      push de
            ld de, 6
            sub 0x20
            call DEMulA
            ex de, hl
            ld hl, (fontTablePtr)
            add hl, de
            ld a, (hl)
            inc hl
        pop de
        ld b, 5
        call putSpriteOR
        add a, d
        ld d, a
_:  pop bc
    pop ix
    pop hl
    pop af
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
;;  The left margin is only required if your string contains newlines.
drawCharAND:
    push af
    push hl
    push ix
    push bc
        cp '\n'
        jr nz, _
        ld a, e
        add a, 6
        ld e, a
        ld d, b
        jr ++_
    
_:      push de
            ld de, 6
            sub 0x20
            call DEMulA
            ex de, hl
            ld hl, (fontTablePtr)
            add hl, de
            ld a, (hl)
            inc hl
        pop de
        ld b, 5
        call putSpriteAND
        add a, d
        ld d, a
_:  pop bc
    pop ix
    pop hl
    pop af
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
;;  The left margin is only required if your string contains newlines.
drawCharXOR:
    push af
    push hl
    push ix
    push bc
        cp '\n'
        jr nz, _
        ld a, e
        add a, 6
        ld e, a
        ld d, b
        jr ++_
    
_:        push de
            ld de, 6
            sub 0x20
            call DEMulA
            ex de, hl
            ld hl, (fontTablePtr)
            add hl, de
            ld a, (hl)
            inc hl
        pop de
        ld b, 5
        call putSpriteXOR
        add a, d
        ld d, a
_:  pop bc
    pop ix
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
;;  D, E: Advanced to position of next character
;; Notes:
;;  The left margin is only required if your string contains newlines.
drawStr:
    push hl
    push af
_:      ld a, (hl)
        or a
        jr z, _
        call drawChar
        inc hl
        jr -_
_:  pop af
    pop hl
    ret

;; drawStrAND [Text]
;;  Draws a zero-delimited string to the screen buffer using AND logic (turns pixels OFF).
;; Inputs:
;;  IY: Screen buffer
;;  HL: String
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Advanced to position of next character
;; Notes:
;;  The left margin is only required if your string contains newlines.
drawStrAND:
    push hl
    push af
_:      ld a, (hl)
        or a
        jr z, _
        call drawCharAND
        inc hl
        jr -_
_:  pop af
    pop hl
    ret

;; drawStrXOR [Text]
;;  Draws a zero-delimited string to the screen buffer using XOR logic (inverts pixels).
;; Inputs:
;;  IY: Screen buffer
;;  HL: String
;;  D, E: X, Y
;;  B: Left margin
;; Outputs:
;;  D, E: Advanced to position of next character
;; Notes:
;;  The left margin is only required if your string contains newlines.
drawStrXOR:
    push hl
    push af
_:      ld a, (hl)
        or a
        jr z, _
        call drawCharXOR
        inc hl
        jr -_
_:  pop af
    pop hl
    ret
    ret

;; drawStrFromStream [Text]
;;  Draws a zero-delimited string to the screen buffer using OR logic (turns pixels ON).
;; Inputs:
;;  IY: Screen buffer
;;  B: Stream ID
;;  D, E: X, Y
;; Outputs:
;;  Stream is advanced to byte after 0.
drawStrFromStream:
    push af
_:      call streamReadByte
        jr nz, _
        or a
        jr z, _
        call drawChar
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
   call dispha
   ret
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
        call DEMulA
        ex de, hl
        ld hl, (fontTablePtr)
        add hl, de
        ld a, (hl)
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
    
#include "font.asm"