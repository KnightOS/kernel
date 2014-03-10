; This is the sentinel byte for all RLE routines.
runIndicator .equ 0x9B

;; rleCompress [Miscellaneous]
;;  Compresses data using a simple Run-Length-Encoding scheme.
;;  All bytes in a compressed block are treated as literal,
;;  except the two following a sentinel byte (selected because of
;;  its low occurance in z80 code), which specify the length of
;;  the run and the byte to run with, respectively.
;; Inputs:
;;  HL: Data to compress
;;  DE: Destination, cannot (yet) be the same location as original data
;;  BC: Size of uncompressed data
;; Outputs:
;;  AF: Destroyed
;;  BC: Size of compressed data
rleCompress:
    push hl
    push de
    push ix
.next:                              ; Must have at least four bytes left in input to try to run.
        ld a, b \ or a \ jr nz, .nextfour
        ld a, c \ or a \ jr z, .done
        cp 4 \ jr c, .literalLast3
.nextfour:                          ; Must have a run of at least four bytes to save space.
        push hl \ pop ix
        ld a, (hl)
        cp (ix + 1)
        jr nz, .literal
        cp runIndicator              ; Except if the run is of the sentinel; then we only need a 2-byte run.
        jr z, .run
        cp (ix + 2)
        jr nz, .literal
        cp (ix + 3)
        jr nz, .literal
.run:
        ; Find the length of the run.
        push de
            ld e, a             ; Save the running byte
            ld d, 0             ; D is the length of the run
_:          inc hl
            dec bc
            inc d
            cp (hl)
            jr nz, _
            ld a, d
            cp 255              ; Check for maximum run length
            ld a, e
            jp nz, -_
_:          ex (sp), hl
            ; DEstination now in HL
            ld (hl), runIndicator
            inc hl
            ld (hl), d
            inc hl
            ld (hl), e
            inc hl
        pop de
        ex de, hl
        jr .next
.literalLast3:
        ld a, (hl)
.literal:
        ld (de), a
        inc de
        cp runIndicator          ; Check if byte is the sentinel, if so it must be escaped.
        jr nz, _
        ld a, 1
        ld (de), a
        inc de
        ld a, runIndicator
        ld (de), a
        inc de
_:      inc hl
        dec bc
        jr .next
.done:
    pop ix

    ; Calculate size of compressed data, store in BC
    ex de, hl
    pop de
    xor a
    sbc hl, de
    ex (sp), hl
    pop bc
    xor a
    ret

;; rleCalculateCompressedLength [Miscellaneous]
;;  Calculates the size of data resulting from a compression, but
;;  does not actually compress anything.
;; Inputs:
;;  HL: Data to compress
;;  BC: Size of decompressed data
;; Outputs:
;;  AF: Destroyed
;;  BC: Size of compressed data
rleCalculateCompressedLength:
    push hl
    push de
    push ix
        ld de, 0
.next:                              ; Must have at least four bytes left in input to try to run.
        ld a, b \ or a \ jr nz, .nextfour
        ld a, c \ or a \ jr z, .done
        cp 4 \ jr c, .literalLast3
.nextfour:                          ; Must have a run of at least four bytes to save space.
        push hl \ pop ix
        ld a, (hl)
        cp (ix + 1)
        jr nz, .literal
        cp runIndicator              ; Except if the run is of the sentinel; then we only need a 2-byte run.
        jr z, .run
        cp (ix + 2)
        jr nz, .literal
        cp (ix + 3)
        jr nz, .literal
.run:
        ; Find the length of the run.
        push de
            ld e, a             ; Save the running byte
            ld d, 0             ; D is the length of the run
_:          inc hl
            dec bc
            inc d
            cp (hl)
            jr nz, _
            ld a, d
            cp 255              ; Check for maximum run length
            ld a, e
            jp nz, -_
_:      pop de
        inc de
        inc de
        inc de
        jr .next
.literalLast3:
        ld a, (hl)
.literal:
        inc de
        cp runIndicator          ; Check if byte is the sentinel, if so it must be escaped.
        jr nz, _
        inc de
        inc de
_:      inc hl
        dec bc
        jr .next
.done:
    pop ix
    push de \ pop bc
    pop de
    pop hl
    xor a
    ret

;; rleDecompress [Miscellaneous]
;;  Decompresses data compressed with the algorithm used by the kernel
;;  routine rleCompress.  See its documentation for algorithm details.
;; Inputs:
;;  HL: Data to decompress
;;  DE: Destination, cannot be the same location as original data
;;  BC: Size of compressed data
;; Outputs:
;;  AF: Destroyed
;;  BC: Size of decompressed data
rleDecompress:
    push hl
    push de
.repeat:
        ld a, b \ or c \ jr z, .done
        ld a, (hl)
        cp runIndicator              ; Check for sentinel byte
        jr z, .expand
        ld (de), a                  ; Copy literal
        jr .nextBytes
.expand:
        dec bc
        inc hl
        push bc
            ld b, 0
            ld c, (hl)
            dec c
            inc hl
            ld a, (hl)
            ld (de), a
            ld a, c
            or a
            jr z, _                 ; Test for length one, special case
            push hl
                push de \ pop hl
                inc de
                ldir
                dec de
            pop hl
_:      pop bc
        dec bc
.nextBytes:
        inc hl
        inc de
        dec bc
        jr .repeat
.done:
    ex de, hl                       ; Calculate size of decompressed data
    pop de
    xor a
    sbc hl, de
    ex (sp), hl
    pop bc
    xor a
    ret

;; rleCalculateDecompressedLength [Miscellaneous]
;;  Calculates the size of data resulting from a decompression, but
;;  does not actually decompress anything.
;; Inputs:
;;  HL: Data to decompress
;;  BC: Size of compressed data
;; Outputs:
;;  AF: Destroyed
;;  BC: Size of decompressed data
rleCalculateDecompressedLength:
    push hl
    push de
    ld de, 0
        ; Just step through and add everything up!
.nextByte:
        ld a, b \ or c \ jr z, .done    ; If we're out of bytes, we're done!
        ld a, (hl)
        inc de                          ; Output gets larger
        dec bc                          ; Input gets smaller
        inc hl                          ; Next input byte
        cp runIndicator
        jr nz, .nextByte
        dec de                          ; Sentinel byte doesn't affect output size
        ld a, e
        add a, (hl)                     ; Add size of run
        ld e, a \ jr nc, _ \ inc d
_:      inc hl
        inc hl                          ; Skip byte to run with (we don't care)
        dec bc
        dec bc
        jr .nextByte
.done:
    push de \ pop bc
    pop de
    pop hl
    xor a
    ret
