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

sha1_hash:
    .db $67,$45,$23,$01
    .db $EF,$CD,$AB,$89
    .db $98,$BA,$DC,$FE
    .db $10,$32,$54,$76
    .db $C3,$D2,$E1,$F0
sha1_length:
    .db $00,$00,$00,$00
    .db $00,$00,$00,$00
sha1_block_ptr:
    .dw sha1_block

sha1_pad:
    ; append the bit '1' to the message
    ; append 0 <= k < 512 bits '0', so that the resulting message length (in bits)
    ;    is congruent to 448 = -64 (mod 512)
    ld a, $80
sha1_pad_zero:
    call sha1_add_byte_no_length
    ld a, (sha1_block_ptr)
    cp (sha1_block+56) & 255
    ld a, $00
    jr nz, sha1_pad_zero
    ; append length of message (before padding), in bits, as 64-bit big-endian integer
    ld hl, sha1_length
    ld de, (sha1_block_ptr)
    ld bc, 8
    ldir
    jr sha1_process_block

sha1_add_byte:
    push af
    ld hl, sha1_length+7
    ld a, (hl)
    add a, 8
    ld (hl), a
    jr nc, _length_ok
_length_inc:
    dec hl
    inc (hl)
    jr z, _length_inc
_length_ok:
    pop af

sha1_add_byte_no_length:
    ld de, (sha1_block_ptr)
    ld (de), a
    inc de
    ld (sha1_block_ptr), de
    ld a, e
    cp (sha1_block+64) & 255
    ret nz
sha1_process_block:

    ;    Extend the sixteen 32-bit words into eighty 32-bit words:
    ;    for i from 16 to 79
    ;        w[i] = (w[i-3] xor w[i-8] xor w[i-14] xor w[i-16]) leftrotate 1
    ld ix, sha1_block+63
    ld c, 64
_extend:
    ld b, 4
_extend_inner:
    inc ix
    ld a, (ix + -12)
    xor (ix + -32)
    xor (ix + -56)
    xor (ix + -64)
    ld (ix), a
    djnz _extend_inner
    push ix
    pop hl
    ld a, (ix + -3)
    rlca
    rl (hl) \ dec hl
    rl (hl) \ dec hl
    rl (hl) \ dec hl
    rl (hl) \ dec hl
    dec c
    jr nz, _extend

    ;    Initialize hash value for this chunk:
    ;    a = h0
    ;    b = h1
    ;    c = h2
    ;    d = h3
    ;    e = h4
    ld hl, sha1_hash
    ld de, sha1_a
    ld bc, 20
    ldir

    ;    Main loop
    ld hl, sha1_block-1 \ ld (sha1_block_ptr), hl
    ld hl, operation_mux \ call sha1_20rounds \ .db $5A,$82,$79,$99
    ld hl, operation_xor \ call sha1_20rounds \ .db $6E,$D9,$EB,$A1
    ld hl, operation_maj \ call sha1_20rounds \ .db $8F,$1B,$BC,$DC
    ld hl, operation_xor \ call sha1_20rounds \ .db $CA,$62,$C1,$D6

    ;    Add this chunk's hash to result so far
    ;    h0 += a
    ;    h1 += b
    ;    h2 += c
    ;    h3 += d
    ;    h4 += e
    ld de, sha1_hash+19
    ld hl, sha1_a+19
    ld c, 5
_add_result:
    call add_32bits
    dec c
    jr nz, _add_result

    ld hl, sha1_block
    ld (sha1_block_ptr), hl
    ret

sha1_20rounds:
    ld (sha1_f_op_ptr), hl
    pop hl
    ld de, sha1_k
    ld bc, 4
    ldir
    push hl

    ld b, 20
_rounds:
    push bc

    ; f = <some operation involving b, c, and d>
    call do_f_operation

    ; temp = (a leftrotate 5) + f + e + k + w[i]
    ld hl, sha1_a
    ld de, sha1_temp
    ld bc, 4
    ldir
    ld a, (sha1_temp)
    rrca
    rrca
    rrca
    rrca
    ld hl, sha1_temp+3
    rld \ rl (hl) \ dec hl
    rld \ rl (hl) \ dec hl
    rld \ rl (hl) \ dec hl
    rld \ rl (hl)
    ld hl, sha1_k+3
    call add_to_temp ; k
    call add_to_temp ; f
    call add_to_temp ; e
    ld hl, (sha1_block_ptr)
    inc hl
    inc hl
    inc hl
    inc hl
    ld (sha1_block_ptr), hl
    call add_to_temp

    ; e = d
    ; d = c
    ; c = b leftrotate 30
    ; b = a
    ; a = temp
    ld hl, sha1_d+3
    ld de, sha1_e+3
    ld bc, 20
    lddr
    ld a, (sha1_c+3)
    ld b, 2
_ror2:
    ld hl, sha1_c
    rrca
    rr (hl) \ inc hl
    rr (hl) \ inc hl
    rr (hl) \ inc hl
    rr (hl)
    djnz _ror2

    pop bc
    djnz _rounds
    ret

do_f_operation:
    ld ix, sha1_a
    ld hl, (sha1_f_op_ptr)
    ld b, 4
    jp (hl)
operation_mux:
    ; f = (b & c) | (~b & d) = ((c ^ d) & 8) ^ d
    ld a, (ix+8)
    xor (ix+12)
    and (ix+4)
    xor (ix+12)
    ld (ix+20), a
    inc ix
    djnz operation_mux
    ret
operation_xor:
    ; f = b ^ c ^ d
    ld a, (ix+4)
    xor (ix+8)
    xor (ix+12)
    ld (ix+20), a
    inc ix
    djnz operation_xor
    ret
operation_maj:
    ; f = (b & c) | (b & d) | (c & d)
    ld c, (ix+4)
    ld d, (ix+8)
    ld e, (ix+12)
    ld a, c
    and d
    ld h, a
    ld a, c
    and e
    ld l, a
    ld a, d
    and e
    or h
    or l
    ld (ix+20), a
    inc ix
    djnz operation_maj
    ret

add_to_temp:
    ld de, sha1_temp+3
add_32bits:
    ld b, 4
    or a
_add_loop:
    ld a, (de)
    adc a, (hl)
    ld (de), a
    dec de
    dec hl
    djnz _add_loop
    ret

sha1_block:
    .block 320

sha1_f_op_ptr:
    .block 2

; Keep these contiguous
sha1_temp: .block 4
sha1_a:    .block 4
sha1_b:    .block 4
sha1_c:    .block 4
sha1_d:    .block 4
sha1_e:    .block 4
sha1_f:    .block 4
sha1_k:    .block 4
