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
            ; IX is registers from before this was called
            ld iy, debugBuffer
            call clearBuffer
            ld de, 0x0A00
            ld hl, debugMenu_top
            rst 0x20
            .dw drawStr
            call debug_drawRegisters
            call fastCopy_skipCheck

            call flushKeys_skipCheck
            call waitKey_skipCheck
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
    ld hl, debugMenu_registers
    ld de, 35
    ld b, 5
    rst 0x20
    .dw drawStr
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
    ld a, (ix + -12)
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
    .db "== Kernel Debugger ==", 0
debugMenu_registers:
    .db "Registers/Flags:\n", 0
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
