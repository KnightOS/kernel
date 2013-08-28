;; crc16 [Crypto]
;;  Performs a Cyclic Redundancy Check on data.
;; Inputs:
;;  HL: Pointer to data
;;  BC: Size of data
;; Outputs:
;;  DE: CRC of data
crc16:
    push hl
    push bc
    ld de, 0xFFFF                        ; initialize crc
.bytelp:
        push bc                          ; save count
            ld a, (hl)                   ; fetch byte from memory
            xor d                        ; xor byte into crc top byte
            ld b, 8                      ; prepare to rotate 8 bits
.rotlp:
            sla e
            adc a, a                     ; rotate crc
            jr nc, _                     ; b15 was zero
            ld d, a                      ; put crc high byte back into d
            ld a, e \ xor 0x21 \ ld e, a ; crc=crc xor &1021, xmodem polynomic
            ld a, d \ xor 0x10           ; and get crc top byte back into a
_:          djnz .rotlp
            ld d, a                      ; put crc top byte back into d

            inc hl
        pop bc
        dec bc
        ld a, b \ or c
        jr nz, .bytelp                   ; loop until num=0
    pop bc
    pop hl
    ret

;; sha1Init [Crypto]
;;  Allocates a memory block to keep the state and result of
;;  a SHA1 hash operation.
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  IX: location of allocated block (on success)
sha1Init:
    push bc
    push de
    push hl
        ld bc, .defaultMemblock_copy_end - .defaultMemblock
        call malloc
        jr nz, .fail
        push ix \ pop de
        ld hl, .defaultMemblock
        ldir

        ; Possibly this could be improved
        push ix
            ld bc, 320 ; SHA1 block is this size
            call malloc
            ; Store the pointers
            push ix \ pop hl
        pop ix
        ld (ix + sha1_block_ptr), l
        ld (ix + sha1_block_ptr + 1), h
        ld (ix + sha1_block_front_ptr), l
        ld (ix + sha1_block_front_ptr + 1), h

        ; Success, set Z
        ld h, a
        xor a
        ld a, h
.fail:
    pop hl
    pop de
    pop bc
    ret

; This is the default memblock.  Its
; state will be changed by the algorithm.
.defaultMemblock:
sha1_hash .equ $ - .defaultMemblock
    .db $67,$45,$23,$01
    .db $EF,$CD,$AB,$89
    .db $98,$BA,$DC,$FE
    .db $10,$32,$54,$76
    .db $C3,$D2,$E1,$F0
; The length of the input is kept here
sha1_length .equ $ - .defaultMemblock
    .db $00,$00,$00,$00
    .db $00,$00,$00,$00
; Keep these contiguous
sha1_temp .equ $ - .defaultMemblock
    .block 4
sha1_a .equ $ - .defaultMemblock
    .block 4
sha1_b .equ $ - .defaultMemblock
    .block 4
sha1_c .equ $ - .defaultMemblock
    .block 4
sha1_d .equ $ - .defaultMemblock
    .block 4
sha1_e .equ $ - .defaultMemblock
    .block 4
sha1_f .equ $ - .defaultMemblock
    .block 4
sha1_k .equ $ - .defaultMemblock
    .block 4
sha1_f_op_ptr .equ $ - .defaultMemblock
    .dw $0000
.defaultMemblock_copy_end:
; Pointers to the SHA1 block are kept here
sha1_block_ptr .equ $ - .defaultMemblock
    .dw $0000
sha1_block_front_ptr .equ $ - .defaultMemblock
    .dw $0000
.defaultMemblock_end:

;; sha1Crypto [Crypto]
;;  Safely deallocates a SHA1 state block allocated by
;;  sha1Init.
;; Inputs:
;;  IX: location of allocated block
sha1Clean:
    push hl
    push ix
        ld l, (ix + sha1_block_front_ptr)
        ld h, (ix + sha1_block_front_ptr + 1)
        call free
        push hl \ pop ix
        call free
    pop ix
    pop hl
    ret

sha1Pad:
    ; append the bit '1' to the message
    ; append 0 <= k < 512 bits '0', so that the resulting message length (in bits)
    ;    is congruent to 448 = -64 (mod 512)
    ld a, $80
.zero:
    call sha1AddByte_noLength
    ld a, (ix + sha1_block_front_ptr)
    add a, 56
    cp (ix + sha1_block_ptr)
    ld a, $00
    jr nz, .zero
    ; append length of message (before padding), in bits, as 64-bit big-endian integer
    push ix \ pop hl
    ld de, sha1_length
    add hl, de
    ld e, (ix + sha1_block_ptr)
    ld d, (ix + sha1_block_ptr + 1)
    ld bc, 8
    ldir
    jr sha1ProcessBlock

sha1AddByte:
    push af
        ld a, (ix + sha1_length + 7)
        add a, 8
        ld (ix + sha1_length + 7), a
        jr nc, .length_ok
        push ix
_:          dec ix
            inc (ix + sha1_length + 7)
            jr z, -_
        pop ix
.length_ok:
    pop af

sha1AddByte_noLength:
    ld e, (ix + sha1_block_ptr)
    ld d, (ix + sha1_block_ptr + 1)
    ld (de), a
    inc de
    ld (ix + sha1_block_ptr), e
    ld (ix + sha1_block_ptr + 1), d
    ld a, (ix + sha1_block_front_ptr)
    add a, 64
    cp e
    ret nz
sha1ProcessBlock:
    ;    Extend the sixteen 32-bit words into eighty 32-bit words:
    ;    for i from 16 to 79
    ;        w[i] = (w[i-3] xor w[i-8] xor w[i-14] xor w[i-16]) leftrotate 1
    ld l, (ix + sha1_block_front_ptr)
    ld h, (ix + sha1_block_front_ptr + 1)
    ld bc, 63
    add hl, bc
    push hl \ ex (sp), iy

        ld c, 64
.extend:
        ld b, 4
.extend_inner:
        inc iy
        ld a, (iy + -12)
        xor (iy + -32)
        xor (iy + -56)
        xor (iy + -64)
        ld (iy), a
        djnz .extend_inner
        push iy \ pop hl
        ld a, (iy + -3)
        rlca
        rl (hl) \ dec hl
        rl (hl) \ dec hl
        rl (hl) \ dec hl
        rl (hl) \ dec hl
        dec c
        jr nz, .extend

        ;    Initialize hash value for this chunk:
        ;    a = h0
        ;    b = h1
        ;    c = h2
        ;    d = h3
        ;    e = h4
        push iy \ pop hl
        ; Unneeded because the sha1_hash offset is 0!
        ;ld de, sha1_hash
        ;add hl, de
        push hl
            ld de, sha1_a
            add hl, de
            ex de, hl
        pop hl
        ld bc, 20
        ldir

        ;    Main loop
        ld l, (ix + sha1_block_front_ptr)
        ld h, (ix + sha1_block_front_ptr + 1)
        dec hl
        ld (ix + sha1_block_ptr), l
        ld (ix + sha1_block_ptr + 1), h
        ld hl, sha1Operation_mux \ call sha1Do20Rounds \ .db $5A,$82,$79,$99
        ld hl, sha1Operation_xor \ call sha1Do20Rounds \ .db $6E,$D9,$EB,$A1
        ld hl, sha1Operation_maj \ call sha1Do20Rounds \ .db $8F,$1B,$BC,$DC
        ld hl, sha1Operation_xor \ call sha1Do20Rounds \ .db $CA,$62,$C1,$D6

        ;    Add this chunk's hash to result so far
        ;    h0 += a
        ;    h1 += b
        ;    h2 += c
        ;    h3 += d
        ;    h4 += e
        push bc
            ; Perhaps this could be improved.
            push ix \ pop de \ push de \ pop hl
            ld bc, 19 + sha1_hash
            add hl, bc
            ex de, hl
            ld bc, 19 + sha1_a
            add hl, bc
            ex de, hl
        pop bc
        ld c, 5
.add_result:
        call sha1_32BitAdd
        dec c
        jr nz, .add_result

        ld l, (ix + sha1_block_front_ptr)
        ld h, (ix + sha1_block_front_ptr + 1)
        ld (ix + sha1_block_ptr), l
        ld (ix + sha1_block_ptr + 1), h
    pop iy
    ret

sha1Do20Rounds:
    ld (ix + sha1_f_op_ptr), l
    ld (ix + sha1_f_op_ptr + 1), h
    ld de, sha1_k
    push ix \ pop hl
    add hl, de
    ex de, hl
    pop hl
ld bc, 4
ldir
    push hl

    ld b, 20
.rounds:
    push bc

        ; f = <some operation involving b, c, and d>
        call .do_f_operation

        ; temp = (a leftrotate 5) + f + e + k + w[i]
        ld bc, 4
        push ix \ pop hl
        ld de, sha1_temp
        add hl, de
        push hl \ pop de
        add hl, bc ; HACK!  This is the correct value to get HL to sha1_a
        ex de, hl
        ldir
        ld a, (ix + sha1_temp)
        rrca
        rrca
        rrca
        rrca
        push de
            ld de, sha1_temp + 3
            push ix \ pop hl
            add hl, de
        pop de
        rld \ rl (hl) \ dec hl
        rld \ rl (hl) \ dec hl
        rld \ rl (hl) \ dec hl
        rld \ rl (hl)
        push de
            ld de, 3 + (sha1_k + 3) - sha1_temp ; Undo the three DECs we just did (HL now
                                                ; at sha1_temp + 3), then add difference
            add hl, de                          ; to get to sha1_k + 3.
        pop de
        call sha1AddToTemp ; k
        call sha1AddToTemp ; f
        call sha1AddToTemp ; e
        ld l, (ix + sha1_block_ptr)
        ld h, (ix + sha1_block_ptr + 1)
        inc hl
        inc hl
        inc hl
        inc hl
        ld (ix + sha1_block_ptr), l
        ld (ix + sha1_block_ptr + 1), h
        call sha1AddToTemp

        ; e = d
        ; d = c
        ; c = b leftrotate 30
        ; b = a
        ; a = temp
        push ix \ pop hl
        ld bc, sha1_d + 3
        add hl, bc
        push hl \ pop de
        inc de \ inc de \ inc de \ inc de ; sha1_e - sha1_d = 4 bytes
        ld bc, 20
        lddr
        ld a, (ix + sha1_c + 3)
        ld b, 2
.ror2:
        push bc
            push ix \ pop hl
            ld bc, sha1_c
            add hl, bc
        pop bc
        rrca
        rr (hl) \ inc hl
        rr (hl) \ inc hl
        rr (hl) \ inc hl
        rr (hl)
        djnz .ror2

    pop bc
    djnz .rounds
    ret

.do_f_operation:
    push ix
        ex (sp), iy
        ex de, hl
        ld de, sha1_a
        add iy, de
        ex de, hl
        ld l, (ix + sha1_f_op_ptr)
        ld h, (ix + sha1_f_op_ptr + 1)
        ld b, 4
        jp (hl)

sha1Operation_mux:
        ; f = (b & c) | (~b & d) = ((c ^ d) & 8) ^ d
        ld a, (iy + 8)
        ld c, (iy + 12)
        xor c
        and (iy + 4)
        xor c
        ld (iy + 20), a
        inc iy
        djnz sha1Operation_mux
        jr sha1Operation_done
sha1Operation_xor:
        ; f = b ^ c ^ d
        ld a, (iy + 4)
        xor (iy + 8)
        xor (iy + 12)
        ld (iy + 20), a
        inc iy
        djnz sha1Operation_xor
        jr sha1Operation_done
sha1Operation_maj:
        ; f = (b & c) | (b & d) | (c & d)
        ;   = (b & c) | ((b | c) & d)
        ld c, (iy + 4)
        ld d, (iy + 8)
        ld a, c
        and d
        ld e, a
        ld a, c
        or d
        and (iy + 12)
        or e
        ld (iy + 20), a
        inc iy
        djnz sha1Operation_maj
        ;jr sha1Operation_done
sha1Operation_done:
    pop iy
    ret

sha1AddToTemp:
    ld de, sha1_temp+3
    push ix
      ex (sp), hl
        add hl, de
        ex de, hl
    pop hl

sha1_32BitAdd:
    ld b, 4
    or a
_:  ld a, (de)
    adc a, (hl)
    ld (de), a
    dec de
    dec hl
    djnz -_
    ret
