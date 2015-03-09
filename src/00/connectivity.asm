initNetwork:
#ifdef LINK_ASSIST
    xor a
    out (PORT_LINKPORT), a
    ; enable R/W link assist and on-byte-reception interrupt generation
    ld a, LINKASSIST_INT_ONRECV
    out (PORT_LINKASSIST_ENABLE), a
    ld a, (0 << BIT_LINKASSIST_SPEED_DIVISOR) + 2
    out (PORT_LINKASSIST_SPEED0), a
    out (PORT_LINKASSIST_SPEED1), a
#else
    ; enable R link assist as the 73/83+ BE only has read support
    ld a, LINKPORT_ASSIST_ACTIVE
    out (PORT_LINKPORT), a
#endif
    ld a, IO_STATE_IDLE
    ld (IOstate), a
    ld hl, 0
    ld (IOIsSending), hl ; set currentIOFrame at the same time
    ld (IODataChecksum), hl ; set IOTransferErrored at the same time
    ld (willSendNextIOFrame), hl ; set temp_io_var at the same time
    ld (busyIOFrame), hl ; set currentIODataByte at the same time
    ld hl, IOFramesQueue
    ld b, 8
    ld de, 6
_:
    ld (hl), IOinactive
    add hl, de
    djnz -_
    ret

;; sendIOFrame [Connectivity]
;;  Pushes a frame to the IO queue for sending.
;; Inputs:
;;  B: size of data
;;  DE: port
;;  HL: pointer to data
sendIOFrame:
    push bc \ push de \ push hl
        push hl
            ld a, (currentIOFrame)
            call dropNextIOFrame
            ld hl, IOFramesQueue
            ; an IO frame is 6 bytes
            push bc
                add a, a
                ld c, a
                add a, a
                add a, c
                ld c, a
                ld b, 0
                add hl, bc
            pop bc
            ; header
            ld (hl), IOoutFrame
            inc hl
            ; port
            ld (hl), e
            inc hl
            ld (hl), d
            inc hl
            ; frame length
            ld (hl), b
            inc hl
        pop de
        ; address of content
        ld (hl), e
        inc hl
        ld (hl), d
        ld hl, currentIOFrame
        ld a, (hl)
        inc (hl)
    pop hl \ pop de \ pop bc
    ret

; Returns usable frame ID in A
dropNextIOFrame:
    push bc \ push hl
        inc a
        cp maxIOFrames
        jr c, +_
        xor a
_:
        ld b, a
        add a, a
        ld c, a
        add a, a
        add a, c
        ld c, a
        ld a, b
        ld b, 0
        ld hl, IOFramesQueue
        add hl, bc    
        bit BIT_IOFrameNeedsClaiming, (hl)
        jr z, .exit
        ld (hl), IOinactive
        ; free content
        push ix
            ld bc, 4
            add hl, bc
            ld ixl, (hl)
            inc hl
            ld ixh, (hl)
            call free
        pop ix
.exit:
    pop hl \ pop bc
    ret
    
; Inputs:
;  DE: port
;  HL: where to start searching - 6
; Outputs:
;  HL: pointer to IO frame on success
;  Z: set on success, reset on failure
;  A: header on success or error code on failure
searchForFrame:
    push bc \ push de
        ld bc, 6
        add hl, bc
        ld b, 8
.nextFrame:
        ld a, IOinactive
        cp (hl)
        jr z, .skip
        push bc
            ; get port
            inc hl
            ld c, (hl)
            inc hl
            ld b, (hl)
            dec hl \ dec hl
            call cpBCDE
        pop bc
        jr z, .found
.skip:
        push de
            ld de, 6
            add hl, de
        pop de
        djnz .nextFrame
        ld a, errIOUnassignedPort
        or a
        jr .exit
.found:
        ld a, (hl)
.exit:
    pop de \ pop bc
    ret

;; getIOFrame [Connectivity]
;;  Check if a specified port has received an IO frame.
;; Inputs:
;;  DE: port
;; Outputs:
;;  HL: pointer to data
;;  C: frame length
;;  Z: set on success, reset on failure
;;  A: 0 on success, error code on failure
;; Notes:
;;  Remember to free HL when you're done with it.
getIOFrame:
    push de
        ld hl, IOFramesQueue - 6
_:
        call searchForFrame
        jr nz, .exit
        bit BIT_IOinFrame, a
        jr z, -_
        bit BIT_IOFrameBusy, a
        jr z, +_
        ld a, errIOFrameNotReady
        jr .exit
_:
        ld (hl), IOinactive
        inc hl \ inc hl \ inc hl
        ld c, (hl)
        inc hl
        ld a, (hl)
        inc hl
        ld h, (hl)
        ld l, a
        xor a
.exit:
    pop de
    ret

;; IOTransferCompleted [Connectivity]
;;  Returns whether or not the port has completed a successful send or reception.
;; Inputs:
;;  DE: port
;; Outputs:
;;  Z: set if the transfer has completed, reset in any other situation
;; Notes:
;;  Z being reset can mean several things, including that the port is unassigned.
IOTransferCompleted:
    push bc \ push hl
        ld hl, IOFramesQueue - 6
_:
        call searchForFrame
        jr nz, .exit
        bit BIT_IOFrameBusy, a
        jr nz, -_
.exit:
    pop hl \ pop bc
    ret
    
