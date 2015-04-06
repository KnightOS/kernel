;; crc16_init [Maths]
;;  Initializes a Cyclic Redundancy Check.
;; Outputs:
;;  DE: Fresh CRC value
crc16_init:
    ld de, 0xffff
    ret
    
;; crc16_updateByte [Maths]
;;  Updates the current CRC16 with the CRC16 of a single byte.
;; Inputs:
;;  A: byte to process
;;  DE: Current CRC16
;; Outputs:
;;  DE: Updated CRC16
crc16_updateByte:
    push af \ push bc \ push hl
        ex de, hl
        ld e, a
        ld b, 8
.byteLoop:
        ld a, h
        xor e
        add hl, hl
        jr nc, .noXOR
        ; HL = HL xor 0x68da
        ld a, l
        xor 0xda
        ld l, a
        ld a, h
        xor 0x68
        ld h, a
.noXOR:
        rl e
        djnz .byteLoop
        ex de, hl
    pop hl \ pop bc \ pop af
    ret
    
;; crc16_updateBuffer [Maths]
;;  Updates the current CRC16 with the CRC16 of a buffer of given size.
;; Inputs:
;;  HL: Pointer to buffer
;;  DE: Current CRC16
;;  BC: Number of bytes to process
;; Outputs:
;;  DE: Updated CRC16
crc16_updateBuffer:
    push af \ push bc \ push hl
        ld a, c
        dec bc
        inc b
        ld c, b
        ld b, a
.loop:
        ld a, (hl)
        call crc16_updateByte
        inc hl
        djnz .loop
        dec c
        jp nz, .loop
    pop hl \ pop bc \ pop af
    ret
