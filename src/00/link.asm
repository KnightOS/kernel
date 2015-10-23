; Machine IDs

#define MID_PC_82       0x02
#define MID_PC_83       0x03
#define MID_PC_8X       0x23
#define MID_CALC_8X     0x73
#define MID_CALC_82     0x82
#define MID_CALC_83     0x83
#define MID_TI_KEYBOARD 0xE0

; Command IDs

#define CMD_KEYBOARD_INPUT  0x01
#define CMD_KEYPAD_INPUT    0xA6

la_reset_buffer:
    xor a
    ld (la_header_ix), a
    ret

la_handleInterrupt:
    bit BIT_LA_INT_RX_DONE, a
    jr nz, .rx_done
    jp sysInterruptDone
.rx_done:
    in a, (PORT_LINK_ASSIST_RX_BUFFER) ; ack
    call la_rx_handle_byte
    jp sysInterruptDone

la_check_timeout:
    push af
    push hl
    push de
    push bc
        ld a, (la_header_ix)
        or a
        jr z, .done ; Skip if this is the first byte of a packet

        ld hl, (la_last_byte_time)
        ld bc, 200 ; 2 seconds ish
        add hl, bc
        ex de, hl
        ld hl, (kernel_current_time)
        sbc hl, de
        jr c, .done
        xor a
        ld (la_header_ix), a ; drop packet
.done:
        ld hl, (kernel_current_time)
        ld (la_last_byte_time), hl
    pop bc
    pop de
    pop hl
    pop af
    ret

la_rx_handle_byte:
    call la_check_timeout
    ld b, a
    ld a, (la_header_ix)
    cp 4
    jr z, la_handle_data
    ld hl, la_header_buffer
    add a, l \ ld l, a \ jr nc, $+3 \ inc h
    ld (hl), b
    ld hl, la_header_ix
    inc (hl)
.handle_header_part:
    ld a, (la_header_buffer) ; machine ID
    ld b, a
    ld hl, la_header_handlers
.header_handler_find:
    ld a, (hl)
    inc hl
    cp 0xFF
    jp z, la_reset_buffer
    cp b
    jr z, .found
    inc hl \ inc hl
    jr .header_handler_find
.found:
    ld b, (hl)
    ld a, (la_header_ix)
    cp b
    ret nz
    inc hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    ex hl, de
    jp (hl)
.not_found:
    jp la_reset_buffer
la_handle_data:
    ; TODO
    jp la_reset_buffer

la_header_handlers:
    ;db machine id, expected header length
    ;dw handler
    .db MID_TI_KEYBOARD, 3
    .dw handle_keyboard_header
    .db 0xFF

handle_keyboard_header:
    call la_reset_buffer
    ld a, (la_header_buffer + 1) ; cmd
    cp CMD_KEYBOARD_INPUT
    ret nz
    ld a, (la_header_buffer + 2) ; scan code
    jp push_scan_code
