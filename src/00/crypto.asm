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
    ld de, 0xFFFF                    ; initialize crc
.bytelp:
        push bc                          ; save count
            ld a, (hl)                   ; fetch byte from memory
            ; the following code updates the crc in de with the byte in a
            xor d                        ; xor byte into crc top byte
            ld b, 8                      ; prepare to rotate 8 bits
.rotlp:
            sla e
            adc a, a                     ; rotate crc
            jp nc, _                     ; b15 was zero
            ld d, a                      ; put crc high byte back into d
            ld a, e \ xor 0x21 \ ld e, a ; crc=crc xor &1021, xmodem polynomic
            ld a, d \ xor 0x10           ; and get crc top byte back into a
_:          dec b \ jr nz, .rotlp        ; loop for 8 bits
            ld d, a                      ; put crc top byte back into d

            inc hl                       ; step to next byte
        pop bc
        dec bc                           ; num=num-1
        ld a, b \ or c
        jr nz, .bytelp                   ; loop until num=0
    pop bc
    pop hl
    ret
