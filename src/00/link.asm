; Machine IDs

#define TIMEOUT_SECS 1

#define MID_PC_82               0x02
#define MID_PC_83               0x03
#define MID_PC_8X               0x23
#define MID_CALC_8X             0x73
#define MID_CALC_82             0x82
#define MID_CALC_83             0x83
#define MID_TI_KEYBOARD         0xE0
#define MID_KOS_RESERVED_MIN    0x40 ; Reserved for kernel use
#define MID_KOS_RESERVED_MAX    0x5F ; Reserved for kernel use
#define MID_KOS_KERNEL          0x5F
; To the best of my knowledge, TI does not use 0x40-0x5F

#define CMD_ACK                 0x56
#define CMD_ERR                 0x5A
#define CMD_CTS                 0x09

io_reset_buffer:
    call io_kill_timeout
    push af
        xor a
        ld (io_header_ix), a
        ld (io_bulk_len), a
        ld (io_bulk_len + 1), a
    pop af
    ret

la_handleInterrupt:
    bit BIT_LA_INT_RX_DONE, a
    jr nz, .rx_done
    bit BIT_LA_INT_TX_DONE, a
    jp nz, io_tx_ready
    bit BIT_LA_INT_ERROR, a
    jr nz, .la_error
    jp sysInterruptDone
.rx_done:
    in a, (PORT_LINK_ASSIST_RX_BUFFER) ; ack
    call io_rx_handle_byte
    jp sysInterruptDone
.la_error:
    in a, (PORT_LINK_ASSIST_ENABLE)
    res BIT_LA_ENABLE_INT_TX, a
    out (PORT_LINK_ASSIST_ENABLE), a
    ld hl, 0
    ld (io_send_remain), hl ; abort tx
    call io_reset_buffer ; abort rx
    jp sysInterruptDone

initIO:
    ld bc, default_header_handlers_end - default_header_handlers
    call malloc
    ld a, 0xFE
    call reassignMemory ; make permanent
    push ix \ pop de
    ld hl, default_header_handlers
    ldir
    ld (io_header_handlers), ix
    ld a, 0xFF
    ld (io_tx_header_ix), a ; ready to send
    ret

default_header_handlers:
    ;db machine id, expected header length, thread ID
    ;dw handler
    .db MID_TI_KEYBOARD, 3, 0xFE
    .dw handle_keyboard_header
    .db MID_KOS_KERNEL, 4, 0xFE
    .dw handle_internal_packets
    .db 0xFF
default_header_handlers_end:

;; ioRegisterHandler [I/O]
;;  Registers an I/O packet handler for a given machine ID.
;; Inputs:
;;  A: Machine ID
;;  B: Expected header length
;;  IX: Callback
;; Notes:
;;  The "expected header length" is the length of the packet header at the time
;;  you want your callback told about it. The maximum is 4.
;;  
;;  Your callback will be called with HL set to the address of the packet header.
;;  
;;  Your callback will be called during an interrupt, so make it short and sweet.
;;  If you return HL != 0 and BC != 0, we'll assume that HL is a pointer to a
;;  buffer to be filled with BC bytes from the rest of the packet. You may set IX
;;  to a callback to run when the full packet is read, or to 0 otherwise.
;;  
;;  If a handler has already been reserved with this machine ID, it will take
;;  precedence.
;;
;;  Your header handler may be called multiple times before the final callback is
;;  invoked. This may occur if we have to retry the packet due to a corrupted
;;  header.
ioRegisterHandler:
    push hl
    push de
    push bc
    ld c, a
    ld a, i
    push af
    ld a, c
    di
        push af
            push bc
                push ix
                push ix
                    ld ix, (io_header_handlers)
                    ld b, (ix + -1)
                    ld c, (ix + -2)
                    inc bc \ inc bc \ inc bc
                    inc bc \ inc bc
                    call realloc ; Add 5 bytes to header handlers
                    ; TODO: OOM
                    ld (io_header_handlers), ix
                    call memSeekToEnd
                    push ix \ pop hl
                    ld (hl), 0xFF \ dec hl
                pop de
                pop ix
                ld (hl), d \ dec hl
                ld (hl), e \ dec hl
                call getCurrentThreadID
                ld (hl), a \ dec hl
            pop bc
            ld (hl), b \ dec hl
        pop af
        ld (hl), a
    pop af
    jp po, _
    ei
_:
    pop bc
    pop de
    pop hl
    ret

; TODO: remove handlers when the owning thread exits

;; ioSendPacket [I/O]
;;  Sends a DBus packet via the I/O port.
;; Inputs:
;;  HL: Buffer (or zero)
;;  BC: Buffer length (or zero)
;;  D: Command ID
;;  E: Machine ID
;;  IX: Callback (or zero)
;; Outputs:
;;  Z: Set if successful, reset if I/O is busy
;; Notes:
;;  This is an asyncronous operation. Your callback will be invoked
;;  (outside the context of your thread) when the send is complete.
;;  Please keep your callback short and sweet, as it will be called
;;  during an interrupt. You are advised to suspend your main thread
;;  or use [[condWait]] and have your callback resume/notify it.
;;  
;;  Attempts to access the buffer memory before the callback is run
;;  will end in tears.
ioSendPacket:
    push bc
    push af
    ld a,i
    di
    push af
        ; io_tx_header_ix is 0xFF when ready to send
        ld a, (io_tx_header_ix)
        cp 0xFF \ jr nz, .abort
        ld a, (io_header_ix)
        or a \ jr nz, .abort
        ld a, (io_bulk_len)
        or a \ jr nz, .abort
        ld a, (io_bulk_len + 1)
        or a \ jr nz, .abort
.checks_pass:
    push hl
    push de
        xor a
        cp c \ jr nz, _
        cp b \ jr nz, _
        ld hl, 0 ; NULL out the send queue if len=0
_:      ld (io_tx_header), de
        ld (io_tx_header + 2), bc
        ld (io_send_queue), hl
        ld (io_send_queue_bak), hl
        ld (io_send_remain), bc
        ld (io_send_callback), ix
        ld (io_tx_header_ix), a
        ld hl, 0
        ld (io_checksum), hl

        call getCurrentThreadId
        ld (io_send_callback_thread), a

        in a, (PORT_LINK_ASSIST_ENABLE)
        set BIT_LA_ENABLE_INT_TX, a
        out (PORT_LINK_ASSIST_ENABLE), a
    pop de
    pop hl
    pop af
    jp po, _
    ei
_:
    pop af
    pop bc
    cp a
    ret
.abort:
    pop af
    jp po, _
    ei
_:
    pop af \ ld b, a \ or 1 \ ld a, b
    pop bc \ ret ; Packet in progress, GTFO

io_reset_timeout:
#ifdef CRYSTAL_TIMERS
    push af
        ld a, CRYS_FREQ_8HZ
        out (PORT_CRYS1_FREQ), a
        ld a, CRYS_LOOP_INT
        out (PORT_CRYS1_LOOP), a
        ld a, TIMEOUT_SECS * 8
        out (PORT_CRYS1_COUNTER), a
    pop af
#endif
    ret

io_kill_timeout:
#ifdef CRYSTAL_TIMERS
    push af
        xor a
        out (PORT_CRYS1_FREQ), a
        out (PORT_CRYS1_LOOP), a
        out (PORT_CRYS1_COUNTER), a
    pop af
#endif
    ret

io_timer_expired:
    ; ACK interrupt
    ld a, CRYS_FREQ_0
    out (PORT_CRYS1_FREQ), a
    ld a, CRYS_LOOP_INT
    out (PORT_CRYS1_COUNTER), a
    ; What were we doing?
    in a, (PORT_LINK_ASSIST_ENABLE)
    bit BIT_LA_ENABLE_INT_RX, a
    jr z, .rx_timeout
    bit BIT_LA_ENABLE_INT_TX, a
    jr z, .tx_timeout
    ; We aren't in the middle of anything. Weird.
    jp sysInterruptDone
.rx_timeout:
.tx_timeout:
    jp sysInterruptDone

io_tx_ready:
    ld a, (io_tx_header_ix)
    cp 0xFF
    jr z, .tx_complete
    cp 4
    jr c, .tx_header
    ld hl, (io_send_queue)
    ld bc, (io_send_remain)
    ld de, 0
    call cpHLDE
    jr z, .tx_complete ; HL=0, then done
    call cpBCDE
    jr z, .tx_checksum ; BC=0, then send checksum
    ld a, (hl)
    out (PORT_LINK_ASSIST_TX_BUFFER), a
    ld de, (io_checksum)
    add a, e \ ld e, a \ jr nc, $+3 \ inc d
    ld (io_checksum), de
    inc hl \ dec bc
    ld (io_send_queue), hl
    ld (io_send_remain), bc
    call cpBCDE
    jr z, .tx_complete
    jp sysInterruptDone
.tx_header:
    ld hl, io_tx_header
    add a, l \ ld l, a \ jr nc, $+3 \ inc h
    ld a, (hl)
    out (PORT_LINK_ASSIST_TX_BUFFER), a
    ld hl, io_tx_header_ix
    inc (hl)
    jp sysInterruptDone
.tx_checksum:
    ld hl, io_checksum
    sub a, 4
    add a, l \ ld l, a \ jr nc, $+3 \ inc h
    ld a, (hl)
    out (PORT_LINK_ASSIST_TX_BUFFER), a
    ld hl, io_tx_header_ix
    inc (hl)
    ld a, (hl)
    cp 6
    jp nz, sysInterruptDone
.tx_complete:
    in a, (PORT_LINK_ASSIST_ENABLE)
    res BIT_LA_ENABLE_INT_TX, a
    out (PORT_LINK_ASSIST_ENABLE), a

    ; Was this packet a simple packet? If so, run callback now
    ld hl, (io_send_queue_bak)
    ld bc, 0
    call cpHLBC
    ld hl, sysInterruptDone
    push hl
    ret nz

    ld a, 0xFF
    ld (io_tx_header_ix), a

    ld hl, (io_send_callback)
    push hl
        call cpHLBC
        ret nz
    pop hl
    ret

io_rx_handle_byte:
    call io_reset_timeout
    ld b, a
    ld hl, (io_bulk_len)
    xor a
    cp h \ jp nz, .handle_bulk_byte
    cp l \ jp nz, .handle_bulk_byte
    ld a, (io_header_ix)
    ld hl, io_header_buffer
    add a, l \ ld l, a \ jr nc, $+3 \ inc h
    ld (hl), b
    ld hl, io_header_ix
    inc (hl)
.handle_header_part:
    ld a, (io_header_buffer) ; machine ID
    ld b, a
    ld hl, (io_header_handlers)
.header_handler_find:
    ld a, (hl)
    inc hl
    cp 0xFF
    jp z, io_reset_buffer
    cp b
    jr z, .found
    inc hl \ inc hl \ inc hl \ inc hl
    jr .header_handler_find
.found:
    ld b, (hl)
    ld a, (io_header_ix)
    cp b
    ret nz
    inc hl
    call getCurrentThreadId
    push af
        ld a, (hl) ; Thread ID
        ld (io_bulk_callback_thread), a
        call setCurrentThread

        inc hl
        ld e, (hl)
        inc hl
        ld d, (hl)
        ex hl, de ; HL to callback

        ld bc, .return_point
        push bc
        push hl

        ld hl, io_header_buffer
        ret
.return_point:
    pop af
    call setCurrentThread

    ld de, 0
    call cpBCDE
    jp z, io_reset_buffer
    call cpHLDE
    jp z, io_reset_buffer

    ; Bulk mode
    inc bc \ inc bc ; checksum
    ld (io_bulk_buffer), hl
    ld (io_bulk_buffer_bak), hl
    ld (io_bulk_len), bc
    ld (io_bulk_callback), ix
    ret
.not_found:
    jp io_reset_buffer
.handle_bulk_byte:
    ld a, b
    ld bc, (io_bulk_len)
    dec bc
    ld (io_bulk_len), bc
    ld hl, 2
    scf \ ccf
    sbc hl, bc
    jr z, .bulk_data
    jr nc, .checksum_data
.bulk_data:
    ld hl, (io_bulk_buffer)
    ld (hl), a
    inc hl
    ld (io_bulk_buffer), hl
    ret
.checksum_data:
    ld hl, io_checksum + 1
    sbc hl, bc
    ld (hl), a
    ld hl, 0
    sbc hl, bc
    ret nz
    ; Confirm checksum is accurate
    ld de, 0
    ld hl, (io_bulk_buffer_bak) ; Start
    ld bc, (io_bulk_buffer)     ; End
.checksum_loop:
    ld a, (hl)
    add a, e \ ld e, a \ jr nc, $+3 \ inc d
    inc hl
    call cpHLBC
    jr nz, .checksum_loop
    ld hl, (io_checksum)
    call cpHLDE
    jr z, .send_ack
.send_err:
    call io_reset_buffer
    ld bc, 0
    ld ix, 0
    ld de, 0x5A5F ; ERR
    jp ioSendPacket
.send_ack:
    call io_reset_buffer
    ld bc, 0
    ld ix, .execute_callback
    ld de, 0x565F ; ACK
    jp ioSendPacket
.execute_callback:
    call getCurrentThreadID
    push af
        ld hl, .return_point_bulk
        push hl

        ld hl, (io_bulk_callback)
        ld a, (io_bulk_callback_thread)

        call setCurrentThread
        push hl
        ret
.return_point_bulk:
    pop af
    call setCurrentThread
    jp io_reset_buffer

handle_internal_packets:
    ; TODO: Don't freak out if we receive an unexpected ACK
    inc hl
    ld a, (hl) ; command type
    cp CMD_ACK
    jr z, .ack
    cp CMD_ERR
    jr z, .err
    ret
.ack: ; Success, run callback
    ld bc, 0
    ld a, 0xFF
    ld (io_tx_header_ix), a

    call getCurrentThreadId
    push af
        ld a, (io_send_callback_thread)
        call setCurrentThread

        ld hl, .ack_rp
        push hl
        ld hl, (io_send_callback)
        push hl
            call cpHLBC
            ret nz
        pop hl
        ret
.ack_rp:
    pop af
    call setCurrentThread
    ld bc, 0 \ ld hl, 0
    ret
.err: ; Checksum error, resend
    call getCurrentThreadId
    push af
        ld a, (io_send_callback_thread)
        call setCurrentThread

        ld hl, .ack_rp
        push hl
        ld hl, (io_send_queue_bak)
        ld de, (io_tx_header)
        ld bc, (io_tx_header + 2)
        push af \ push bc
        jp checks_pass@ioSendPacket
