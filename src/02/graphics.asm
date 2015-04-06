;; getPixel [Display]
;;  Finds the address of and mask for a pixel on the screen buffer.
;; Inputs:
;;  IY: Screen buffer
;;  A,L: X, Y
;; Outputs:
;;  HL: Address of pixel
;;  A: Mask
;; Notes:
;;  If the pixel is on, HL & A is nonzero.
getPixel:
    push bc
        push af
            ld b, a
            xor a
            ld h, a
            ld c, l
    
            add hl, hl
            add hl, hl
            and h
            jr nz, .outOfBounds
            ld a, b
            ld b, h
            rra
            add hl, bc
            rra
            add hl, hl
            rra
            add hl, bc
            add hl, bc
            cp 12
            jr nc, .outOfBounds
            add a, l
            ld l, a
            jr nc, $+3
            inc h
            push iy \ pop bc
            add hl, bc
        pop af
        and 7
        ld b, a
        ld a, 0x80
        jr z, _
        rrca
        djnz $-1
_:  pop bc
    ret      
.outOfBounds:
        pop af
    pop bc
    ld hl, 0
    xor a
    ret


;; setPixel [Display]
;;  Sets (turns on) a pixel on the screen buffer.
;; Inputs:
;;  IY: Screen buffer
;;  A,L: X, Y
setPixel:
    push hl
    push af
        call getPixel
        or (hl)
        ld (hl), a
    pop af
    pop hl
    ret

;; resetPixel [Display]
;;  Sets (turns on) a pixel on the screen buffer.
;; Inputs:
;;  IY: Screen buffer
;;  A,L: X, Y
resetPixel:
    push hl
    push af
        call getPixel
        cpl
        and (hl)
        ld (hl), a
    pop af
    pop hl
    ret

;; invertPixel [Display]
;;  Inverts a pixel on the screen buffer.
;; Inputs:
;;  IY: Screen buffer
;;  A,L: X, Y
invertPixel:
    push hl
    push af
        call getPixel
        xor (hl)
        ld (hl), a
    pop af
    pop hl
    ret

;; drawVLine [Display]
;;  Draws a vertical line on the screen buffer using OR (turns pixels ON) logic.
;;  Does clipping.
;; Inputs:
;;  IY: screen buffer
;;  A, L: X, Y
;;  C: height
drawVLine:
    push af \ push bc \ push de \ push hl
        ld b, a
        ld a, 64
        sub l
        cp c
        jr c, .exitEarly
        ld a, b
        call getPixel
        ld b, a
        ld a, h
        or l
        jr z, .exitEarly
        ld a, b
        ld b, c
        ld c, a
        ld de, 12
.vline_loop:
        ld a, c
        or (hl)
        ld (hl), a
        add hl, de
        djnz .vline_loop
.exitEarly:
    pop hl \ pop de \ pop bc \ pop af
    ret

;; drawVLineAND [Display]
;;  Draws a vertical line on the screen buffer using AND (turns pixels OFF) logic.
;;  Does clipping.
;; Inputs:
;;  IY: screen buffer
;;  A, L: X, Y
;;  C: height
drawVLineAND:
    push af \ push bc \ push de \ push hl
        ld b, a
        ld a, 64
        sub l
        cp c
        jr c, .exitEarly
        ld a, b
        call getPixel
        cpl
        ld b, a
        ld a, h
        or l
        jr z, .exitEarly
        ld a, b
        ld b, c
        ld c, a
        ld de, 12
.vline_loop:
        ld a, c
        and (hl)
        ld (hl), a
        add hl, de
        djnz .vline_loop
.exitEarly:
    pop hl \ pop de \ pop bc \ pop af
    ret 

;; rectXOR [Display]
;;  Draws a filled rectangle on the screen buffer using XOR (invert) logic.
;; Inputs:
;;  IY: Screen buffer
;;  E, L: X, Y
;;  C, B: Width, height
rectXOR: ; by Quigibo
    push af
    push bc
    push hl
    push de
        call _
    pop de
    pop hl
    pop bc
    pop af
    ret
_:  ld a, 96        ;Clip Top
    sub e
    ret c
    ret z
    cp c            ;Clip Bottom
    jr nc, $ + 3
    ld c, a
    ld a, 64        ;Clip Left
    sub l
    ret c
    ret z
    cp b            ;Clip Right
    jr nc, $ + 3
    ld b, a

    xor a           ;More clipping...
    cp b
    ret z
    cp c
    ret z
    ld h,a
    ld d,a

    push bc
        push iy \ pop bc
        ld a, l
        add a, a
        add a, l
        ld l, a
        add hl, hl
        add hl, hl        ;(e,_) = (X,Y)
        add hl, bc        ;(_,_) = (width,height)

        ld a, e
        srl e
        srl e
        srl e
        add hl, de
        and 0b00000111    ;(a,_) = (X^8,Y)
    pop de                ;(e,d) = (width,height)

    ld b, a
    add a, e
    sub 8
    ld e, 0
    jr c, .boxInvSkip
    ld e, a
    xor a
.boxInvSkip:

.boxInvShift:            ;Input:  b = Left shift
    add a, 8             ;Input:  a = negative right shift
    sub b                ;Output: a = mask
    ld c, 0
.boxInvShift1:
    scf
    rr c
    dec a
    jr nz, .boxInvShift1
    ld a, c
    inc b
    rlca
.boxInvShift2:
    rrca
    djnz .boxInvShift2
.boxInvLoop1:                ;(e, d) = (width, height)
    push hl              ;     a = bitmask
        ld b, d
        ld c, a
        push de
            ld de, 12
.boxInvLoop2:
            ld a, c
            xor (hl)
            ld (hl), a
            add hl, de
            djnz .boxInvLoop2
        pop de
    pop hl
    inc hl
    ld a, e
    or a
    ret z
    sub 8
    ld e, b
    jr c, .boxInvShift
    ld e, a
    ld a, 0b11111111
    jr .boxInvLoop1
    ret

;; rectAND [Display]
;;  Draws a filled rectangle on the screen buffer using AND (turns pixels OFF) logic.
;; Inputs:
;;  IY: Screen buffer
;;  E, L: X, Y
;;  C, B: Width, height
rectAND:
    push    af
    push    hl
    push    bc
    push    de
        call    .rectAND
    pop de
    pop bc
    pop hl
    pop af
    ret
.rectAND:
    ld a, 96        ;Clip Top
    sub e
    ret c
    ret z
    cp c            ;Clip Bottom
    jr nc, $ + 3
    ld c, a
    ld a, 64        ;Clip Left
    sub l
    ret c
    ret z
    cp b            ;Clip Right
    jr nc, $ + 3
    ld b,a

    xor a           ;More clipping...
    cp b
    ret z
    cp c
    ret z
    ld h, a
    ld d, a

    push bc
        push iy \ pop bc
        ld a, l
        add a, a
        add a, l
        ld l, a
        add hl, hl
        add hl, hl        ;(e,_) = (X,Y)
        add hl, bc        ;(_,_) = (width,height)

        ld a, e
        srl e
        srl e
        srl e
        add hl, de
        and 0b00000111    ;(a,_) = (X^8,Y)
    pop de                ;(e,d) = (width,height)

    ld b, a
    add a, e
    sub 8
    ld e, 0
    jr c, .boxANDskip
    ld e, a
    xor a
.boxANDskip:

.boxANDshift:            ;Input:  b = Left shift
    add a, 8             ;Input:  a = negative right shift
    sub b                ;Output: a = mask
    ld c, 0
.boxANDshift1:
    scf
    rr c
    dec a
    jr nz, .boxANDshift1
    ld a, c
    inc b
    rlca
.boxANDshift2:
    rrca
    djnz .boxANDshift2

.boxANDloop1:            ;(e,d) = (width,height)
    push hl              ;    a = bitmask
        ld b, d
        ld c, a
        push de
            ld de, 12
.boxANDloop2:
            ld a, c
            cpl
            and (hl)
            ld (hl), a
            add hl, de
            djnz .boxANDloop2
        pop de
    pop hl
    inc hl
    ld a, e
    or a
    ret z
    sub 8
    ld e, b
    jr c, .boxANDshift
    ld e, a
    ld a, 0b11111111
    jr .boxANDloop1
    ret
