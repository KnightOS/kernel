initRandom:
    push hl
    push af
        setBankA(0x04) ; filesystem, probably pretty unpredictable
        ld hl, 0x4000
        ld a, r \ rrca \ xor (hl) \ inc hl \ ld (random_seed), a
        ld a, r \ rrca \ xor (hl) \ inc hl \ ld (random_seed), a
        ld a, r \ rrca \ xor (hl) \ inc hl \ ld (random_seed), a
        ld a, r \ rrca \ xor (hl) \ inc hl \ ld (random_seed), a
        ld a, r \ rrca \ xor (hl) \ inc hl \ ld (random_seed), a
        ld a, r \ rrca \ xor (hl) \ inc hl \ ld (random_seed), a
        ld a, r \ rrca \ xor (hl) \ inc hl \ ld (random_seed), a
        ld a, r \ rrca \ xor (hl) \ inc hl \ ld (random_seed), a
    pop af
    pop hl
    ret

;; getRandom [Miscellaneous]
;;  Gets an 8-bit random number.
;; Outputs:
;;  A: Random number (0-255)
;; Notes:
;;  This is not cryptographically random.
getRandom:
    push hl
    push de
    push bc
        ld hl, random_seed+4
        ld e, (hl)
        inc hl
        ld d, (hl)
        inc hl
        ld c, (hl)
        inc hl
        ld a, (hl)
        ld b, a
        rl e \ rl d
        rl c \ rla
        rl e \ rl d
        rl c \ rla
        rl e \ rl d
        rl c \ rla
        ld h, a
        rl e \ rl d
        rl c \ rla
        xor b
        rl e \ rl d
        xor h
        xor c
        xor d
        ld hl, random_seed+6
        ld de, random_seed+7
        ld bc, 7
        lddr
        ld (de), a
    pop bc
    pop de
    pop hl
    ret
