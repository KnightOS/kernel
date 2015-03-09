;; cpHLDE [Maths]
;;  Compares HL to DE.
;; Output:
;;  Same as z80 CP instruction.
cpHLDE:
    or a
    sbc hl, de
    add hl,de
    ret
;; cpHLBC [Maths]
;;  Compares HL to BC.
;; Output:
;;  Same as z80 CP instruction.
cpHLBC:
    or a
    sbc hl, bc
    add hl,bc
    ret
;; cpBCDE [Maths]
;;  Compares DE to BC.
;; Output:
;;  Same as z80 CP instruction.
cpBCDE:
    push hl
    ld h, b
    ld l, c
    or a
    sbc hl, de
    pop hl
    ret
    
;; smul16By8 [Maths]
;;  Performs a signed multiplication of A and DE.
;; Inputs:
;;  A: Multiplier
;;  DE: Multiplicand
;; Outputs:
;;  HL: Product of A and DE.
smul16By8:
    push bc
        ld hl, 0
        ld bc, 0x0800
        or a
        jp p, .loop
        sbc hl, de
.loop:
        add hl, hl
        rla
        jr nc, $ + 4
        add hl, de
        adc a, c
        djnz .loop
    pop bc
    ret

;; mul8By8 [Maths]
;;  Performs an unsigned multiplication of H and E
;; Inputs:
;;  H: Multiplier
;;  E: Multiplicand
;; Outputs:
;;  HL: Product of H and E.
mul8By8:
    push de
        ld l, 0
        ld d, l

        sla h
        jr nc, $ + 3
        ld l, e

.macro mul8By8Iter
        add hl, hl
        jr nc, $ + 3
        add hl, de
.endmacro
        mul8By8Iter
        mul8By8Iter
        mul8By8Iter
        mul8By8Iter
        mul8By8Iter
        mul8By8Iter
        mul8By8Iter
.undefine mul8By8Iter
    pop de
    ret

;; mul16By8 [Maths]
;;  Performs an unsigned multiplication of A and DE.
;; Inputs:
;;  A: Multiplier
;;  DE: Multiplicand
;; Outputs:
;;  AHL: Product of A and DE.
mul16By8:
    push bc
        ld hl, 0
        ld c, l

        add a, a
        jr nc, $ + 4
        ld h, d
        ld l, e

.macro mul16By8Iter
        add hl, hl
        rla
        jr nc, $ + 4
        add hl, de
        adc a, c
.endmacro
        mul16By8Iter
        mul16By8Iter
        mul16By8Iter
        mul16By8Iter
        mul16By8Iter
        mul16By8Iter
        mul16By8Iter
.undefine mul16By8Iter
    pop bc
    ret

;; mul16By16 [Maths]
;;  Performs an unsigned multiplication of DE and BC.
;; Inputs:
;;  DE: Multiplier
;;  BC: Multiplicand
;; Outputs:
;;  DEHL: Product of DE and BC.
mul16By16:
    ld hl, 0

    sla e
    rl d
    jr nc, $ + 4
    ld h, b
    ld l, c

.macro mul16By16Iter
    add hl, hl
    rl e
    rl d
    jr nc, $ + 6
    add hl, bc
    jr nc, $ + 3
    inc de
.endmacro
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
    mul16By16Iter
.undefine mul16By16Iter
    ret

;; mul32By8 [Maths]
;;  Performs an unsigned multiplication of DEHL and A.
;; Outputs:
;;  DEHL: product of DEHL and A
mul32By8:
    push bc \ push ix
        ld ixl, 8
        push de
            push hl
                ld hl, 0
                ld d, h
                ld e, l
.loop:
                add hl, hl
                rl e
                rl d
                rla
                jr nc, .noAdd
            pop bc
            add hl, bc
            ex (sp), hl
            push hl
                adc hl, de
            pop de
            ex de, hl
            ex (sp), hl
            push bc
.noAdd:
                dec ixl
                jr nz, .loop
            pop bc
        pop bc
    pop ix \ pop bc
    ret
    
;; div32By16 [Maths]
;;  Performs `ACIX = ACIX / DE`
;; Outputs:
;;  ACIX: ACIX / DE
;;  HL: Remainder
;;  B: 0
div32By16:
    ld hl, 0
    ld b, 32
.loop:
    add ix, ix
    rl c
    rla
    adc hl,hl
    jr  c, .overflow
    sbc hl,de
    jr  nc, .setBit
    add hl,de
    djnz .loop
    ret
.overflow:
    or a
    sbc hl,de
.setBit:
    inc ixl
    djnz .loop
    ret

;; div8By8 [Maths]
;;  Performs `D = D / E`
;; Outputs:
;;  D: D / E
;;  A: Remainder
div8By8:
    xor a
    ; Unrolled 8 times
    sla d
    rla
    cp e
    jr c, $+4
    sub e
    inc d
    sla d
    rla
    cp e
    jr c, $+4
    sub e
    inc d
    sla d
    rla
    cp e
    jr c, $+4
    sub e
    inc d
    sla d
    rla
    cp e
    jr c, $+4
    sub e
    inc d
    sla d
    rla
    cp e
    jr c, $+4
    sub e
    inc d
    sla d
    rla
    cp e
    jr c, $+4
    sub e
    inc d
    sla d
    rla
    cp e
    jr c, $+4
    sub e
    inc d
    sla d
    rla
    cp e
    jr c, $+4
    sub e
    inc d
    ret

;; sub16From32 [Maths]
;;  Performs `ACIX = ACIX - DE`
sub16from32:
    push hl
    push de
    push bc
        push ix \ pop hl
        push de
            ld d, a
            ld e, c
        pop bc

        or a
        sbc hl, bc
        jr nc, _
        dec de
_:  push hl \ pop ix
    ld a, d \ ld c, e
    pop bc
    pop de
    pop hl
    ret

;; add16To32 [Maths]
;;  Performs `ACIX = ACIX + DE`
add16to32:
    push hl
        push de
            push ix \ pop hl
            push de
                ld d, a
                ld e, c
            pop bc
            add hl, bc
            jr nc, _
            inc de
_:          push hl \ pop ix
            ld a, d \ ld c, e
        pop de
    pop hl
    ret

;; divHLByC [Maths]
;;  Performs `HL = HL / C`
;; Outputs:
;;  HL: HL / C
;;  A: Remainder
divHLbyC:
    push bc
        xor a
        ld b, 16
_:      add hl, hl
        rla
        cp c
        jr c, $ + 4
        sub c
        inc l
        djnz -_
    pop bc
    ret

;; divACByDE [Maths]
;;  Performs `AC = AC / DE`
;; Outputs:
;;  AC: AC / DE
;;  HL: Remainder
divACbyDE:
   ld hl, 0
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   .db 0xCB, 0x31 ; sll c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   ret

;; sdivACbyDE [Maths]
;;  Performs `AC = AC / DE`. The operation is signed.
;; Output:
;;  AC: AC / DE
;;  HL: Remainder
;; Notes:
;;  B is destroyed
sDivACbyDE:
    xor d
    push af
        xor d
        jp p, .nosign1
        ld b, a
        xor a
        sub c
        ld c, a
        sbc a, a
        sub b
.nosign1:
        ld b, a
        bit 7, d
        jr z, .nosign2
        xor a
        sub e
        ld e, a
        sbc a, a
        sub d
        ld d, a
.nosign2:
        ld a, b
        call divACbyDE
        ld b, a
    pop af
    ld a, b
    ret p
    xor a
    sub c
    ld c, a
    sbc a, a
    sub b
    ret
   
;; smin [Maths]
;;  Returns the smallest between HL and DE. The operation is signed.
;; Inputs:
;;  HL: integer
;;  DE: integer
;; Outputs:
;;  HL: the smallest of the previous HL and DE
;;  DE: the largest of the previous HL and DE
smin:
    ld a, l
    sub e
    ld a, h
    sbc a, d
    rra
    xor d
    xor h
    jp m, $ + 4
    ex de, hl
    ret
    
;; smax [Maths]
;;  Returns the largest between HL and DE. The operation is signed.
;; Inputs:
;;  HL: integer
;;  DE: integer
;; Outputs:
;;  HL: the largest of the previous HL and DE
;;  DE: the smallest of the previous HL and DE
smax:
    ld a, l
    sub e
    ld a, h
    sbc a, d
    rra
    xor d
    xor h
    jp p, $ + 4
    ex de, hl
    ret

;; isin [Maths]
;;  Returns the sine of the given angle.
;; Inputs:
;;  A: Angle
;; Outputs:
;;  A: Sine of the angle, scaled to 64
;; Notes:
;;  The input angle has a period of 256, and the output value will be
;;  in the range [-64, 64] to represent [-1, 1].
;;  
;;  That is, to get the cosine of 180 degrees, pass in 128 and you will
;;  receive 0.
isin:
    sub 0x40 ; a quarter of a period
    ; fall through
    
;; icos [Maths]
;;  Returns the cosine of the given angle.
;; Inputs:
;;  A: Angle
;; Outputs:
;;  A: Sine of the angle, scaled to 64
;; Notes:
;;  The input angle has a period of 256, and the output value will be
;;  in the range [-64, 64] to represent [-1, 1].
;;  
;;  That is, to get the cosine of 180 degrees, pass in 128 and you will
;;  receive -64 (two's compliment).
icos:
    push hl \ push de
        ld hl, .cosLUT
        ld e, a
        ld d, 0
        add hl, de
        ld a, (hl)
    pop de \ pop hl
    ret
    ; scale : 63
.cosLUT:
    .db 63, 63, 63, 63, 63, 63, 63, 63, 62, 62, 62, 61, 61, 60, 60, 59
    .db 59, 58, 57, 57, 56, 55, 54, 54, 53, 52, 51, 50, 49, 48, 47, 46
    .db 45, 44, 42, 41, 40, 39, 38, 36, 35, 34, 32, 31, 30, 28, 27, 25
    .db  24, 23, 21, 20, 18, 17, 15, 14, 12, 10, 9, 7, 6, 4, 3, 1
    .db 0, -1, -3, -4, -6, -7, -9, -10, -12, -14, -15, -17, -18, -20, -21, -23
    .db -24, -25, -27, -28, -30, -31, -32, -34, -35, -36, -38, -39, -40, -41, -42, -44
    .db -45, -46, -47, -48, -49, -50, -51, -52, -53, -54, -54, -55, -56, -57, -57, -58
    .db -59, -59, -60, -60, -61, -61, -62, -62, -62, -63, -63, -63, -63, -63, -63, -63
    .db -63, -63, -63, -63, -63, -63, -63, -63, -62, -62, -62, -61, -61, -60, -60, -59
    .db -59, -58, -57, -57, -56, -55, -54, -54, -53, -52, -51, -50, -49, -48, -47, -46
    .db -45, -44, -42, -41, -40, -39, -38, -36, -35, -34, -32, -31, -30, -28, -27, -25
    .db -24, -23, -21, -20, -18, -17, -15, -14, -12, -10, -9, -7, -6, -4, -3, -1
    .db 0, 1, 3, 4, 6, 7, 9, 10, 12, 14, 15, 17, 18, 20, 21, 23
    .db 24, 25, 27, 28, 30, 31, 32, 34, 35, 36, 38, 39, 40, 41, 42, 44
    .db 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 54, 55, 56, 57, 57, 58
    .db 59, 59, 60, 60, 61, 61, 62, 62, 62, 63, 63, 63, 63, 63, 63, 63

