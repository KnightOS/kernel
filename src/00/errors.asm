; Kernel error screen - give error code in A. Will not return.
kernelError:
    ; This routine assumes that everything is well and truly screwed, so we don't make any assumptions
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

    ld iy, 0xC000
    call clearBuffer
    ; Find the appropriate error message
    ld de, 0
    ld b, 0
    ld hl, errorMessage
    call drawStr
    push af
        call drawHexA ; TODO: Does this destroy A?
    pop af
    ld hl, errorTable
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

errorMessage:
    .db "==Kernel Error ", 0
continueMessage:
    .db "\n\nPress any key to shut down", 0
errorTable:
    .dw init_not_found_text
    .dw no_threads_text

init_not_found_text:
    .db "/bin/init not found", 0
no_threads_text:
    .db "There are no running threads", 0
