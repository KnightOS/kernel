;; crc16_init [Maths]
;;  Initializes a Cyclic Redundancy Check.
;; Notes:
;;  This basically sets DE to 0xffff.
crc16_init:
    ld de, 0xffff
    ret
    
;; crc16_updateByte [Maths]
;;  Updates the current CRC16 with the CRC16 of a single byte.
;; Inputs:
;;  A: byte to process
;;  DE: current CRC16
;; Outputs:
;;  DE: updated CRC16 of byte
crc16_updateByte:
    push af \ push bc \ push hl
        ex de, hl
        ld e, a
        ld b, 8
.byteLoop:
        ld a, h
        xor e
        add hl, hl
        rl a
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
;;  HL: pointer on buffer to process
;;  DE: current CRC16
;;  BC: number of bytes to process
;; Outputs:
;;  DE: updated CRC16 of byte
crc16_updateBuffer:
    push af \ push bc \ push hl
.loop:
        ld a, (hl)
        call crc16_updateByte
        inc hl
        dec bc
        ld a, b
        or c
        jr nz, .loop
    pop hl \ pop bc \ pop af
    ret
    
