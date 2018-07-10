initRandom:
    push hl
    push de
    push af
        setBankA(0x04) ; filesystem,  probably pretty unpredictable
        ld hl, 0x4010
        ld de, random_seed
        ld a, r \ add a, (hl) \ ld l, a  \ ld (de), a \ inc de
        ld a, r \ add a, (hl) \ ld l, a  \ ld (de), a \ inc de
        ld a, r \ add a, (hl) \ ld l, a  \ ld (de), a \ inc de
        ld a, r \ add a, (hl) \ ld l, a  \ ld (de), a \ inc de
        ld a, r \ add a, (hl) \ ld l, a  \ ld (de), a \ inc de
        ld a, r \ add a, (hl) \ ld l, a  \ ld (de), a \ inc de
        ld a, r \ add a, (hl) \ ld l, a  \ ld (de), a \ inc de
        ld a, r \ add a, (hl) \ ld (de), a
    pop af
    pop de
    pop hl
    ret
    
;; getRandom [Miscellaneous]
;;  Gets an 8-bit random number.
;; Outputs:
;;  A: Random number (0-255)
;; Notes:
;;  This is not cryptographically random.
    push hl
    push de
    push bc
        ld hl, (random_seed)
        ld de, (random_seed+2)
        ld b, h
        ld c, l
        add hl, hl \ rl e \ rl d
        add hl, hl \ rl e \ rl d
        inc l
        add hl, bc
        ld (random_seed), hl
        ld hl, (random_seed+2)
        adc hl, de
        ld (random_seed+2), hl
        ex de, hl
        ld hl, (random_seed+4)
        ld de, (random_seed+6)
        ld bc, 54321
        add hl, hl \ rl c \ rl b
        ld (random_seed+6), bc
        sbc a, a
        and %11000101
        xor l
        ld l, a
        ld (random_seed+4), hl
        ld a, d
        add a, b
    pop bc
    pop de
    pop hl
    ret
