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
