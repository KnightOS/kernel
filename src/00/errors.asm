; Kernel error screen - give error code in A. Will not return.
; Set bit 7 of A if it may be possible to recover
kernelError:
    di
    ld sp, 0
    push af
        ; Reset the screen to a usable state
        ld a, 0x05
        call lcdDelay
        out (0x10), a
        ld a, 0x01
        call lcdDelay
        out (0x10), a
        ld a, 3
        call lcdDelay
        out (0x10), a
        ld a, 0x17
        call lcdDelay
        out (0x10), a
        ld a, $B
        call lcdDelay
        out (0x10), a
    pop af

    bit 7, a
    jr z, _
    call allocScreenBuffer
    jr z, ++_
_:  ld iy, 0xC000
    jr _
_:  call clearBuffer
    ; Find the appropriate error message
    ld de, 0
    ld b, 0
    ld hl, errorMessage
    call drawStr
    push af
        call drawHexA ; TODO: Does this destroy A?
    pop af
    ld c, 0
    bit 7, a
    jr z, _
    ld c, 1 ; Save this for later, we can recover from this crash
    res 7, a
_:  ld hl, errorTable
    add a
    add l
    ld l, a
    jr nc, _
    inc h
_:  ld e, (hl)
    inc hl
    ld d, (hl)
    ex de, hl
    ld de, 0x0006
    call drawStr
    ld a, c
    cp 1
    jr z, attemptRecovery
    ld hl, continueMessage
    call drawStr
    ; We could just directly output to the screen and maybe be a
    ; little safer, but we need to clear the screen as well and
    ; this saves enough space to make it worth doing.
    call fastCopy_skipCheck
    call flushkeys_skipCheck
_:  call getKey_skipCheck
    or a
    jr z, -_
    call flushkeys_skipCheck
    jp boot
attemptRecovery:
    ld hl, recoveryMessage
    call drawStr
    call fastCopy_skipCheck
_:  call getKey_skipCheck
    cp kPlus
    jr z, recover
    cp kMinus
    jp z, boot
    jr -_
recover:
    ; Give the first thread in the thread table control and hope for the best
    ld a, (threadTable)
    ld (hwLockLCD), a
    ld (hwLockKeypad), a
    call resumeThread
    ; TODO: Clean up unclaimed memory
    jp contextSwitch_manual

errorMessage:
    .db "==Kernel Error ", 0
continueMessage:
    .db "\n\nPress any key to shut down", 0
recoveryMessage:
    .db "\n\nIt may be possible to recover\n"
    .db "Press + to attempt recovery\n"
    .db "Press - to shut down", 0
errorTable:
    .dw init_not_found_text
    .dw no_threads_text
    .dw no_active_threads_text

init_not_found_text:
    .db "/bin/init not found", 0
no_threads_text:
    .db "There are no running threads", 0
no_active_threads_text:
    .db "There are no active threads", 0
