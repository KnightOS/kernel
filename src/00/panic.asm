; Kernel error screen - give error code in A. Will not return.
; Set bit 7 of A if it may be possible to recover
panic:
    di
    ld sp, 0
    push af
    #ifdef COLOR
        ; Set GPIO config
        ld a, 0xE0
        out (PORT_GPIO_CONFIG), a
        call colorLcdOn
        call clearColorLcd
        call setLegacyLcdMode
    #else
        ; Reset the screen to a usable state
        ld a, 1 + LCD_CMD_AUTOINCDEC_SETX
        call lcdDelay
        out (PORT_LCD_CMD), a
        ld a, 1 + LCD_CMD_SETOUTPUTMODE
        call lcdDelay
        out (PORT_LCD_CMD), a
        ld a, 1 + LCD_CMD_SETDISPLAY
        call lcdDelay
        out (PORT_LCD_CMD), a
        ld a, 7 + LCD_CMD_POWERSUPPLY_SETLEVEL
        call lcdDelay
        out (PORT_LCD_CMD), a
        ld a, 3 + LCD_CMD_POWERSUPPLY_SETENHANCEMENT
        call lcdDelay
        out (PORT_LCD_CMD), a
    #endif
    pop af

    bit 7, a
    jr z, _
    call allocScreenBuffer
    jr z, ++_
_:  ld iy, 0xC000
_:  call clearBuffer
    ; Find the appropriate error message
    ld de, 0
    ld b, 0
    ld hl, errorMessage
    rst 0x20
    .dw drawStr
    push af
        rst 0x20
        .dw drawHexA
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
    rst 0x20
    .dw drawStr
    ld a, c
    cp 1
    jr z, attemptRecovery
    ld hl, continueMessage
    rst 0x20
    .dw drawStr
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
    rst 0x20
    .dw drawStr
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
    .dw library_not_found_text

init_not_found_text:
    .db "/bin/init not found", 0
no_threads_text:
    .db "There are no running threads", 0
no_active_threads_text:
    .db "There are no active threads", 0
library_not_found_text:
    .db "The requested library is not\nloaded", 0
