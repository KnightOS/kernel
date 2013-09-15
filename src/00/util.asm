;; suspendDevice [System]
;;  Turns off the screen, enters low power mode, and halts system operation until the ON key is pressed.
suspendDevice:
    ld a, i
    push af
    ld a, 2
    out (0x10), a ; Disable LCD
    di ; And interrupts, for now
    im 1 ; interrupt mode 1, for cleanliness
    in a, (3)
    push af
        ld a, 1
        out (3), a ; ON
        ei ; Enable interrupting when ON is pressed
        halt ; and halt
        di
    pop af
    out (3), a
    ld a, 3
    out (0x10), a ; Enable the screen
    pop af
    ret po
    ei
    ret

; TODO: This could use some improvement
;; hexToHL [Miscellaneous]
;;  Converts a hexadecimal string to a number.
;; Inputs:
;;  HL: String pointer
;; Outputs:
;;  HL: Value
hexToHL:
    push de
    push af
        ; D
        ld a, (hl)
        or a
        jr z, .done
        call hexToA_doConvert
        rla \ rla \ rla \ rla
        ld d, a \ inc hl
        ld a, (hl)
        or a
        jr z, .done
        call hexToA_doConvert
        or d \ ld d, a \ inc hl
        ; E
        ld a, (hl)
        or a
        jr z, .done
        call hexToA_doConvert
        rla \ rla \ rla \ rla
        ld e, a \ inc hl
        ld a, (hl)
        or a
        jr z, .done
        call hexToA_doConvert
        or e
        ld e, a
        ex de, hl
.done:
    pop af
    pop de
    ret

;; hexToA [Miscellaneous]
;;  Converts a hexadecimal string to a number.
;; Inputs:
;;  HL: String pointer
;; Outputs:
;;  A: Value
hexToA:
    push bc
    push hl
        ld b, 0
_:      ld a, (hl)
        or a
        jr z, hexToA_ret

        rl b \ rl b \ rl b \ rl b
        call hexToA_doConvert
        or b
        ld b, a
        inc hl
        jr -_

hexToA_ret:
        ld a, b
    pop hl
    pop bc
    ret

hexToA_doConvert:
    cp 'a' ; Convert to lowercase
    jr c, _
        sub 'a' - 'A'
_:  cp 'A' ; Close gap between numbers and letter
    jr c, _
        sub 'A'-('9'+1)
_:  sub '0' ; To number
    ret

lcdDelay:
    push af
_:    in a,(0x10)
    rla
    jr c,-_
    pop af
    ret

;; cpHLDE [Miscellaneous]
;;  Compares HL to DE.
;; Output:
;;  Same as z80 CP instruction.
cpHLDE:
    or a
    sbc hl, de
    add hl,de
    ret
;; cpHLBC [Miscellaneous]
;;  Compares HL to BC.
;; Output:
;;  Same as z80 CP instruction.
cpHLBC:
    or a
    sbc hl, bc
    add hl,bc
    ret
;; cpBCDE [Miscellaneous]
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

;; stringLength [Miscellaneous]
;;  Determines the length of a zero delimited string.
;; Inputs:
;;  HL: String pointer
;; Outputs:
;;  BC: String length
stringLength:
    push af
    push hl
        ld bc, 0
        xor a
        cpir
        ; bc = -bc
        ld a, b \ xor $FF \ ld b, a \ ld a, c \ xor $FF \ add a, 1 \ jr nc, $+3 \ inc b \ ld c, a
        dec bc
    pop hl
    pop af
    ret

;; getBatteryLevel [Miscellaneous]
;;  Determines the approximate battery level.
;; Outputs:
;;  B: Battery level
;; Notes:
;;  B is a value from 0 to 4, where 0 is critical and 4 is full.
getBatteryLevel:
#ifdef CPU15
    push af
        ld bc, 0x0403
        ; Reset battery threshold
        in a, (4)
        or 0b11000000
        out (4), a
_:      push bc
            rrc c \ rrc c
            in a, (4)
            and 0b11000000
            or c
            out (4), a
            in a, (2)
            bit 0, a
            jr z, _
        pop bc
        dec c
        djnz _
_:  pop af
    ret
#else
    push af
        in a, (2)
        and 0b11111110
        ld b, a
    pop af
    ret
#endif

;; DEMulA [Miscellaneous]
;;  Performs `HL = DE * A`
DEMulA:
    push bc
    ld hl, 0 ; Use HL to store the product
    ld b, 8 ; Eight bits to check
.loop:
    rrca ; Check least-significant bit of accumulator
    jr nc, .skip ; If zero, skip addition
    add hl, de
.skip:
    sla e ; Shift DE one bit left
    rl d
    djnz .loop
    pop bc
    ret

;; compareStrings [Miscellaneous]
;;  Determines if two strings are equal.
;; Inputs:
;;  HL: String pointer
;;  DE: String pointer
;; Outputs:
;;  Z: Set if equal, reset if not equal
compareStrings:
    ld a, (de)
    or a
    jr z, .end
    cp (hl)
    ret nz
    inc hl
    inc de
    jr compareStrings
.end:
    ld a, (hl)
    or a
    ret

;; stringCopy [Miscellaneous]
;;  Copies a string.
;; Inputs:
;;  HL: String pointer
;;  DE: Destination
stringCopy:
    push de
    push hl
    ex de, hl
_:  ld a, (de)
    ld (hl), a
    or a
    jr z, _
    inc hl \ inc de
    jr -_
_:  pop de
    pop hl
    ret

;; sort [Miscellaneous]
;;  Sorts a specified array of numbers.
;; Inputs:
;;  HL: first element in array
;;  DE: Last element in array
;; Notes:
;;  This routine is an in-place version of a radix sort, which has an O(k*n)
;;  runtime for k-bit numbers.  It also requires a smaller, fixed amount of
;;  stack space.
sort:
    ld b, 0b10000000
.recurse:
    push bc
        push de
            push hl
                or a                    ; We must initially clear CA bit, later it's never modified.
.nextbyte:
                sbc hl, de \ add hl, de ; Check if our bins have met up
                jr c, _
                jr nz, .nextbit         ; If they have, restart with next bit

_:              ld a, (hl)              ; Perform our bit test
                and b
                jr nz, _
                inc hl                  ; It's already in the 0s bin.  0s bin gets larger.
                jr .nextbyte
_:              ld a, (hl)              ; Switch number at top of 1s bin with this one
                ex de, hl
                ld c, (hl)
                ld (hl), a
                ex de, hl
                ld (hl), c
                dec de                  ; 1s bin gets larger.
                jr .nextbyte

.nextbit:
                srl b                   ; Next bit please
                jr c, .done             ; If our carry is 1, we've been through all 8 bits (base case).
            pop hl
            call .recurse               ; Sort the 0s bin
            ex de, hl
            inc hl
        pop de
        call .recurse                   ; Sort the 1s bin
    pop bc
    ret
.done:
            pop hl
        pop de
    pop bc
    ret

;; div32By16 [Miscellaneous]
;;  Performs `ACIX = ACIX / DE`
;; Outputs:
;;  As described above, and:
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

;; sub16From32 [Miscellaneous]
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

;; add16To32 [Miscellaneous]
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
_:  push hl \ pop ix
    ld a, d \ ld c, e
    pop de
    pop bc
    ret

;; divHLByC [Miscellaneous]
;;  Performs `HL = HL / C`
;; Outputs:
;;  As described above, and:
;;  A: Remainder
divHLbyC:
   xor a
   ld b, 16
_: add hl, hl
   rla
   cp c
   jr c, $+4
   sub c
   inc l
   djnz -_
   ret

;; divACByDE [Miscellaneous]
;;  Performs `AC = AC / DE`
;; Outputs:
;;  As described above, and:
;;  HL: Remainder
divACbyDE:
   ld hl, 0
   ld b, 16
_: srl c
   rla
   adc hl, hl
   sbc hl, de
   jr nc, $+4
   add hl, de
   dec c
   djnz -_
   ret

;; getBootCodeVersionString [Miscellaneous]
;;  Gets the version string from the device's boot code.
;; Outputs:
;;  HL: String pointer
;; Notes:
;;  This allocates memory to hold the string. Deallocate it with [[free]] when you no longer need it.
getBootCodeVersionString:
    ld a, i
    push af
    di
        push af
        push bc
        push ix
        push de
            ld a, bootPage
            out (6), a
            ld hl, 0x400F ; Location of boot code version string
            call stringLength
            inc bc
            call malloc
            push ix \ pop de
            ldir
            push ix \ pop hl
        pop de
        pop ix
        pop bc
        pop af
    pop af
    ret po
    ei
    ret

; This is the sentinel byte for all RLE routines.
rleSentinel .equ 0x9B

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
        cp rleSentinel              ; Except if the run is of the sentinel; then we only need a 2-byte run.
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
            ld (hl), rleSentinel
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
        cp rleSentinel          ; Check if byte is the sentinel, if so it must be escaped.
        jr nz, _
        ld a, 1
        ld (de), a
        inc de
        ld a, rleSentinel
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
        cp rleSentinel              ; Except if the run is of the sentinel; then we only need a 2-byte run.
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
        cp rleSentinel          ; Check if byte is the sentinel, if so it must be escaped.
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
        cp rleSentinel              ; Check for sentinel byte
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
        cp rleSentinel
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

;; randA [Miscellaneous]
;;  Returns a random byte in A.
;; Inputs:
;;  A': seed
;; Outputs:
;;  A: random byte
;;  A': reseeded
randA:
    push hl \ push bc
        ex af, af'
        ld a, i
        add a, (hl)
        inc hl
        sbc a, (hl)
        inc hl
        ld c, a
        ld a, r
        xor (hl)
        and 7
        ld b, a
        ld a, c
_:
        rra
        djnz -_
        ld b, a
        ex af, af'
        ld a, b
    pop bc \ pop hl
    ret
