;; waitKey [Input]
;;  Blocks until a key is pressed, then returns that key code.
;; Outputs:
;;  A: Key code
waitKey:
_:  call hasKeypadLock
    jr nz, -_ ; Loop until a lock is acquired
_:  call getKey
    or a
    jr z, -_
    ret

waitKey_skipCheck:
_:  call getKey_skipCheck
    or a
    jr z, -_
    ret

;; flushKeys [Input]
;;  Blocks until all keys are released.
flushkeys:
    call hasKeypadLock
    ret nz
flushkeys_skipCheck:
    push af
    push bc
    ; Done in a loop; runs far too fast on actual hardware
        ld b, 0x80
_:      xor a
        out (PORT_KEYPAD), a
        nop \ nop
        #ifdef COLOR
        nop \ nop
        #endif
        in a, (PORT_KEYPAD)
        inc a
        jr nz, -_
        djnz -_
    pop bc
    pop af
    ret

;; getKey [Input]
;;  Returns the currently pressed key code, or zero if no keys are pressed.
;; Outputs:
;;  A: Key code
getKey:
    call hasKeypadLock
    jr z, _
    xor a
    ret
_:
getKey_skipCheck:
    push bc
    push af
    push de
    push hl
.getK2:
    ld b, 7
.loop:
    ld a, 7
    sub b
    ld hl, .keygroups
    ld d, 0 \ ld e, a
    add hl, de
    ld a, (hl)
    ld c, a

    ld a, 0xFF
    out (PORT_KEYPAD), a
    ld a, c
    out (PORT_KEYPAD), a
    nop \ nop \ nop \ nop
    #ifdef COLOR
    nop \ nop \ nop \ nop
    #endif
    in a, (PORT_KEYPAD)

    ld de,0
    cp 254 \ jr z, .incslide + 7
    cp 253 \ jr z, .incslide + 6
    cp 251 \ jr z, .incslide + 5
    cp 247 \ jr z, .incslide + 4
    cp 239 \ jr z, .incslide + 3
    cp 223 \ jr z, .incslide + 2
    cp 191 \ jr z, .incslide + 1
    cp 127 \ jr z, .incslide

.loopend:
    djnz .loop

    xor a
    ld (kernelGarbage), a
    jr .end
.incslide:
    inc e \ inc e \ inc e \ inc e \ inc e \ inc e \ inc e
    push de
        ld a,7
        sub b
        add a,a \ add a,a \ add a,a
        ld d,0 \ ld e,a
        ld hl, .keygroup1
        add hl, de
    pop de
    add hl, de
    ld a, (hl)

    ld d, a
    ld a,(kernelGarbage)
    cp d \ jr z, .end
    ld a, d
    ld (kernelGarbage), a

.end:
    pop hl
    pop de
    ld b, a
    pop af
    ld a, b
    pop bc
    ret

.keygroups:
    .db 0xFE, 0xFD, 0xFB, 0xF7, 0xEF, 0xDF, 0xBF ;...
.keygroup1:
    .db 0x01, 0x02, 0x03, 0x04, 0x00, 0x00, 0x00, 0x00
.keygroup2:
    .db 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x00
.keygroup3:
    .db 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x00
.keygroup4:
    .db 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20
.keygroup5:
    .db 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28
.keygroup6:
    .db 0x00, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30
.keygroup7:
    .db 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38
