; Kernel debugger
; Access with rst 0x30
debugger:
    push af
    push bc
    push de
    push hl
    push ix
    push iy
        ld ix, 10
        add ix, sp
        ld a, i
        push af
        di
        getBankA
        push af
debugger_main:
            ld iy, debugBuffer
            call clearBuffer

            ; Flash status
            in a, (2)
            bit 2, a
            ld a, ' '
            jr z, _
            ld a, 'F'
_:          ld de, (96 - 4) << 8
            rst 0x20
            .dw drawChar

            ld de, 0
            ld hl, debugMenu_top
            rst 0x20
            .dw drawStr

            ld d, 0x0A
            ld hl, debugMenu
_:          ld c, (hl)
            inc hl
            ld b, (hl)
            inc hl
            inc hl \ inc hl
            push hl
                ld hl, 0xFFFF
                call cpHLBC
                jr z, _
                ld h, b \ ld l, c
                rst 0x20
                .dw drawStr
                ld b, 0x0A
                rst 0x20
                .dw newline
            pop hl
            jr -_

_:          pop hl
            ld de, 0x0606
            ld b, 5
            ld hl, debug_cursorSprite
            call putSpriteOR

            call debug_drawRegisters
            call fastCopy_skipCheck

            call flushKeys_skipCheck
            call waitKey_skipCheck
debug_exit:
        pop af
        setBankA
        pop af
        jp po, _
        ei
_:  pop iy
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ret

debug_kernel:
debug_portmon:
debug_hexEdit:
debug_disassembler:
    jp debugger_main

.macro drawReg(offset)
    rst 0x20
    .dw drawStr
    push hl
        ld l, (ix + offset)
        ld h, (ix + offset + 1)
        rst 0x20
        .dw drawHexHL
    pop hl
    inc hl \ inc hl \ inc hl \ inc hl \ inc hl
    inc d \ inc d
.endmacro
debug_drawRegisters:
    ld de, 41 | (5 << 8)
    ld b, 5
    ld hl, debugMenu_regnames

    drawReg(2) ; PC
    ; SP
    rst 0x20
    .dw drawStr
    push hl
        push ix \ pop hl
        dec hl \ dec hl
        rst 0x20
        .dw drawHexHL
    pop hl
    inc hl \ inc hl \ inc hl \ inc hl \ inc hl
    inc d \ inc d
    drawReg(-6) ; HL 6
    rst 0x20
    .dw newline
    drawReg(-4) ; DE 4
    drawReg(-2) ; BC 2
    drawReg(0) ; AF 0
    rst 0x20
    .dw newline
    drawReg(-8) ; IX 8
    drawReg(-10) ; IY 10
    ; IFF2
    rst 0x20
    .dw drawStr
    ld a, (ix + -13)
    ld hl, debugMenu_iff2reset ; Note: this is intentionally backwards
    bit 2, a
    jr nz, _
    ld hl, debugMenu_iff2set
_:  rst 0x20
    .dw drawStr
    ld a, (ix + 0)
    call debug_drawFlags
    ret

debug_drawFlags:
    ld b, 8
    ld hl, debugMenu_flags
_:  bit 0, a
    call z, .flagReset
    call nz, .flagSet
    inc hl
    inc d \ inc d
    rrca
    djnz -_
    ret
.flagSet:
    push af
        ld a, (hl)
        rst 0x20
        .dw drawChar
    pop af
    ret
.flagReset:
    push af
        ld a, '-'
        rst 0x20
        .dw drawChar
    pop af
    ret

debugMenu_top:
    .db "Kernel Debugger\n", 0
debugMenu_regnames:
    .db "PC: ", 0
    .db "SP: ", 0
    .db "HL: ", 0
    .db "DE: ", 0
    .db "BC: ", 0
    .db "AF: ", 0
    .db "IX: ", 0
    .db "IY: ", 0
    .db "IFF2: ", 0
debugMenu_iff2set:
    .db "0\nFlags:  ", 0
debugMenu_iff2reset:
    .db "1\nFlags:  ", 0
debugMenu_flags:
    .db "CNP*H*ZS"
debugMenu:
    .dw .return, debug_exit
    .dw .hexedit, debug_hexEdit
    .dw .dasm, debug_disassembler
    .dw .portmon, debug_portmon
    .dw .kernel, debug_kernel
    .dw 0xFFFF
.hexedit:
    .db "Hex Editor", 0
.dasm:
    .db "Disassembler", 0
.portmon:
    .db "Port Monitor", 0
.kernel:
    .db "Kernel State", 0
.return:
    .db "Exit Debugger", 0
debug_flashLocked:
    .db "Flash Locked", 0
debug_flashUnlocked:
    .db "Flash Unlocked", 0
debug_cursorSprite:
    .db 0b10000000
    .db 0b11000000
    .db 0b11100000
    .db 0b11000000
    .db 0b10000000
