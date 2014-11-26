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
            cp maxIOFrames
            call nc, dropOldestIOFrame
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
            ld a, IOoutFrame | IOFrameBusy
            ld (hl), a
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

dropOldestIOFrame:
    push hl
        ld hl, IOFramesQueue
        ; check if content has already been claimed
        ; will only be 1 if the frame's content needs clearing
        bit 3, (hl)
        jr z, .exit
        ; free content
        push ix
            inc hl \ inc hl \ inc hl \ inc hl
            ld ixl, (hl)
            inc hl
            ld ixh, (hl)
            call free
        pop ix
.exit:
        xor a
    pop hl
    ret
    
; Inputs:
;  DE: port
; Outputs:
;  HL: pointer to IO frame on success
;  Z: set on success, reset on failure
;  A: header on success or error code on failure
searchForFrame:
    push bc \ push de
        ld hl, IOFramesQueue
        ld b, 8
.nextFrame:
        push bc
            ; get port
            inc hl
            ld c, (hl)
            inc hl
            ld b, (hl)
            call cpBCDE
        pop bc
        dec hl \ dec hl
        jr z, .found
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
;;  Z: set on success, reset on failure
;;  A: 0 on success, error code on failure
;; Notes:
;;  Remember to free HL when you're done with it.
getIOFrame:
    push de
        call searchForFrame
        jr nz, .exit
        bit 2, (hl)
        jr z, $ + 6
        ld a, errIOFrameNotReady
        jr .exit
        ld a, (hl)
        and IOFrameNeedsClaiming ^ 0xFF
        ld (hl), a
        ld de, 4
        add hl, de
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
;;  A: garbage if the transfer has completed, error code in any other situation
;; Notes:
;;  Z being reset can mean several things, including that the port is unassigned.
;;  A will be an error code if Z is reset.
IOTransferCompleted:
    push bc \ push hl
        call searchForFrame
        jr nz, .exit
        bit 2, a
        jr z, .exit
        ld a, errIOFrameNotReady
.exit:
    pop hl \ pop bc
    ret
    
