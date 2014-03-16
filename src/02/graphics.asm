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
    push de
    push bc
        ld h, 0
        ld d, h
        ld e, l
    
        add hl, hl
        add hl, de
        add hl, hl
        add hl, hl
    
        ld e, a
        srl e
        srl e
        srl e
        add hl, de

        push iy \ pop de
        push bc
            ld bc, 0x300
            call cpHLBC
        pop bc
        jr nc, .outOfBounds
        add hl, de
        and 7
        ld b, a
        ld a, 0x80
        jr z, _

        rrca
        djnz $-1
_:  pop bc
    pop de
    ret
.outOfBounds:
        ld hl, 0
        xor a
    pop bc
    pop de
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

;; drawLine [Display]
;;  Draws a line on the screen buffer using OR (turns pixels ON) logic.
;; Inputs:
;;  IY: Screen buffer
;;  D, E: X1, Y1
;;  H, L: X2, Y2
;; Notes:
;;  This function does not clip lines to the screen boundaries.
drawLine: ; By James Montelongo
    push hl
    push de
    push bc
    push af
    push ix
    push iy
        call .drawLine
    pop iy
    pop ix
    pop af
    pop bc
    pop de
    pop hl
    ret

.drawLine:
        ld a, h
        cp d
        jp nc, .noswapx
        ex de, hl
.noswapx:

        ld a, h
        sub d
        jp nc, .posx
        neg
.posx:
        ld b, a
        ld a, l
        sub e
        jp nc, .posy
        neg
.posY:
        ld c, a
        ld a, l
        ld hl, -12
        cp e
        jp c, .lineup
        ld hl, 12
.lineup:
        ld ix, .xbit
        ld a, b
        cp c
        jp nc, .xline
        ld b, c
        ld c, a
        ld ix, .ybit
.xline:
        push hl
            ld a, d
            ld d, 0
            ld h, d
            sla e
            sla e
            ld l, e
            add hl, de
            add hl, de
            ld e, a
            and 0b00000111
            srl e
            srl e
            srl e
            add hl, de
            push iy \ pop de
            ;ld de, gbuf
            add hl, de
            add a, a
            ld e, a
            ld d, 0
            add ix, de
            ld e, (ix)
            ld d, (ix + 1)
            push hl \ pop ix
            ex de, hl
        pop de \ push hl
        ld h, b
        ld l, c
        ld a, h
        srl a
        inc b
        ret

.xbit:
    .dw .drawX0, .drawX1, .drawX2, .drawX3
    .dw .drawX4, .drawX5, .drawX6, .drawX7
.ybit:
    .dw .drawY0, .drawY1, .drawY2, .drawY3
    .dw .drawY4, .drawY5, .drawY6, .drawY7
    
.drawX0:
        set 7, (ix)
        add a, c
        cp h
        jp c, $ + 3 + 2 + 1
        add ix, de
        sub h
        djnz .drawX1
        ret
.drawX1:
        set 6, (ix)
        add a, c
        cp h
        jp c, $ + 3 + 2 + 1
        add ix, de
        sub h
        djnz .drawX2
        ret
.drawX2:
        set 5, (ix)
        add a, c
        cp h
        jp c, $ + 3 + 2 + 1
        add ix, de
        sub h
        djnz .drawX3
        ret
.drawX3:
        set 4, (ix)
        add a, c
        cp h
        jp c, $ + 3 + 2 + 1
        add ix, de
        sub h
        djnz .drawX4
        ret
.drawX4:
        set 3, (ix)
        add a, c
        cp h
        jp c, $ + 3 + 2 + 1
        add ix, de
        sub h
        djnz .drawX5
        ret
.drawX5:
        set 2, (ix)
        add a, c
        cp h
        jp c, $ + 3 + 2 + 1
        add ix, de
        sub h
        djnz .drawX6
        ret
.drawX6:
        set 1, (ix)
        add a, c
        cp h
        jp c, $ + 3 + 2 + 1
        add ix, de
        sub h
        djnz .drawX7
        ret
.drawX7:
        set 0, (ix)
        inc ix
        add a, c
        cp h
        jp c, $ + 3 + 2 + 1
        add ix, de
        sub h
        djnz .drawX0
        ret

.drawY0_:
        inc ix
        sub h
        dec b
        ret z
.drawY0:
        set 7, (ix)
        add ix, de
        add a, l
        cp h
        jp nc, .drawY1_
        djnz .drawY0
        ret
.drawY1_:
        sub h
        dec b
        ret z
.drawY1:
        set 6, (ix)
        add ix, de
        add a, l
        cp h
        jp nc, .drawY2_
        djnz .drawY1
        ret
.drawY2_:
        sub h
        dec b
        ret z
.drawY2:
        set 5, (ix)
        add ix, de
        add a, l
        cp h
        jp nc, .drawY3_
        djnz .drawY2
        ret
.drawY3_:
        sub h
        dec b
        ret z
.drawY3:
        set 4, (ix)
        add ix, de
        add a, l
        cp h
        jp nc, .drawY4_
        djnz .drawY3
        ret
.drawY4_:
        sub h
        dec b
        ret z
.drawY4:
        set 3, (ix)
        add ix, de
        add a, l
        cp h
        jp nc, .drawY5_
        djnz .drawY4
        ret
.drawY5_:
        sub h
        dec b
        ret z
.drawY5:
        set 2, (ix)
        add ix, de
        add a, l
        cp h
        jp nc, .drawY6_
        djnz .drawY5
        ret
.drawY6_:
        sub h
        dec b
        ret z
.drawY6:
        set 1, (ix)
        add ix, de
        add a, l
        cp h
        jp nc, .drawY7_
        djnz .drawY6
        ret
.drawY7_:
        sub h
        dec b
        ret z
.drawY7:
        set 0, (ix)
        add ix, de
        add a, l
        cp h
        jp nc, .drawY0_
        djnz .drawY7
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

;; rectOR [Display]
;;  Draws a filled rectangle on the screen buffer using OR (turns pixels ON) logic.
;; Inputs:
;;  IY: Screen buffer
;;  E, L: X, Y
;;  C, B: Width, height
rectOR:
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
    ld b, a

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
    jr c, .boxORskip
    ld e, a
    xor a
.boxORskip:

.boxORshift:            ;Input:  b = Left shift
    add a, 8            ;Input:  a = negative right shift
    sub b               ;Output: a = mask
    ld c, 0
.boxORshift1:
    scf
    rr c
    dec a
    jr nz, .boxORshift1
    ld a, c
    inc b
    rlca
.boxORshift2:
    rrca
    djnz .boxORshift2

.boxORloop1:            ;(e,d) = (width,height)
    push hl             ;    a = bitmask
        ld b, d
        ld c, a
        push de
            ld de, 12
.boxORloop2:
            ld a, c
            or (hl)
            ld (hl), a
            add hl, de
            djnz .boxORloop2
        pop de
    pop hl
    inc hl
    ld a,e
    or a
    ret z
    sub 8
    ld e, b
    jr c, .boxORshift
    ld e, a
    ld a, 0b11111111
    jr .boxORloop1
    ret

;; rectAND [Display]
;;  Draws a filled rectangle on the screen buffer using AND (turns pixels OFF) logic.
;; Inputs:
;;  IY: Screen buffer
;;  E, L: X, Y
;;  C, B: Width, height
rectAND:
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
