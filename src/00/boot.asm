; Calculator boot-up code
boot:
    di
    jr _
;; shutdown [System]
;;  Shuts off the device.
;; Notes:
;;  This will never return. Call it with `jp shutdown`
shutdown:
    ; TODO: Crash detection
_:  di

    ld a, 6
    out (4), a ; Memory mode 0

    #ifdef FLASH4MB
    xor a
    out (0x0E), a
    out (0x0F), a
    #endif

    #ifdef CPU15
    ; Set memory mapping
    ; Bank 0: Flash Page 00
    ; Bank 1: Flash Page *
    ; Bank 2: RAM Page 01
    ; Bank 3: RAM Page 00 ; In this order for consistency with TI-83+ and TI-73 mapping
    ld a, 0x81
    out (7), a
    #else
    ; Set memory mapping
    ; Bank 0: Flash Page 00
    ; Bank 1: Flash Page *
    ; Bank 2: RAM Page 01
    ; Bank 3: RAM Page 00
    ld a, 0x41
    out (7), a
    #endif

    ld sp, userMemory ; end of kernel garbage

#ifndef TEST
    call suspendDevice
#endif
;; reboot [System]
;;  Restarts the device.
;; Notes:
;;  This will never return. Call it with `jp reboot`
reboot:
    di

    ld sp, userMemory ; end of kernel garbage

    #ifdef FLASH4MB
    xor a
    out (0x0E), a
    out (0x0F), a
    #endif
    ; Re-map memory
    ld a, 6
    out (4), a
    #ifdef CPU15
    ld a, 0x81
    out (7), a
    #else
    ld a, 0x41
    out (7), a
    #endif

    ; Manipulate protection states
    #ifdef CPU15 ; TI-83+ SE, TI-84+, TI-84+ SE, TI-84+ CSE
        call unlockFlash
            ; Remove RAM Execution Protection
            xor a
            out (0x25), a ; RAM Lower Limit ; out (25), 0
            dec a
            out (0x26), a ; RAM Upper Limit ; out (26), $FF

            ; Remove Flash Execution Protection
            out (0x23), a ; Flash Upper Limit ; out (23), $FF
            out (0x22), a ; Flash Lower Limit ; out (22), $FF
        call lockFlash

        ; Set CPU speed to 15 MHz
        ld a, 1
        out (0x20), a

    #else ; TI-73, TI-83+
        #ifndef TI73 ; RAM does not have protection on the TI-73

        ; Remove RAM/Flash protection
        call unlockFlash
            xor a
            out (5), a
            out (0x16), a

            ld a, 0b000000001
            out (5), a
            xor a
            out (0x16), a

            ld a, 0b000000010
            out (5), a
            xor a
            out (0x16), a

            ld a, 0b000000111
            out (5), a
            xor a
            out (0x16), a
        call lockFlash
        #endif
    #endif

    ; Set intterupt mode
    ld a, 0b000001111
    out (3), a

    ; Clear RAM
    ld hl, 0x8000
    ld (hl), 0
    ld de, 0x8001
    ld bc, 0x7FFF
    ldir

    call formatMem

    ; Initialize the font table pointer
    ld hl, kernel_font
    ld (fontTablePtr), hl

    ; Set all file handles to unused
    ld hl, fileHandleTable
    ld (hl), 0xFF
    ld de, fileHandleTable + 1
    ld bc, 8 * maxFileStreams
    ldir

    ld a, threadRangeMask ; When the first thread is allocated, this will wrap to 0
    ld (lastThreadId), a

    #ifdef COLOR
    ; Set GPIO config
    ld a, 0xE0
    out (0x39), a
    call colorLcdOn
    call clearColorLcd
    call setLegacyLcdMode
    #else
    ; Initialize LCD
    ld a, 0x05
    call lcdDelay
    out (0x10), a ; X-Increment Mode

    ld a, 0x01
    call lcdDelay
    out (0x10), a ; 8-bit mode

    ld a, 3
    call lcdDelay
    out (0x10), a ; Enable screen

    ld a, 0x17 ; versus 0x13? TIOS uses 0x17, and that's the only value that works (the datasheet says go with 0x13)
    call lcdDelay
    out (0x10), a ; Op-amp control (OPA1) set to max (with DB1 set for some reason)

    ld a, 0xB ; B
    call lcdDelay
    out (0x10), a ; Op-amp control (OPA2) set to max

    ; Different amounts of contrast look better on different models
    #ifdef USB
        ld a, 0xEF
    #else
        #ifdef TI73
            ld a, 0xFB
        #else
            ld a, 0xF4
        #endif
    #endif

    ld (currentContrast), a
    call lcdDelay
    out (0x10), a ; Contrast
    #endif

#ifdef TEST
    jp testrunner
#endif

    call testProgram

    ld de, bootFile
    call fileExists
    ld a, kerr_init_not_found
    jp nz, kernelError
    call launchProgram
    ld h, 0
    call setInitialA

    jp contextSwitch_manual

bootFile:
    .db "/bin/init", 0
testFile:
    .db "/test", 0

testProgram:
    call allocScreenBuffer
    xor a
    ld (currentContrast), a
    ; Default values here are a sanity check... of the sanity check
    ld a, fatStart
    setBankA
    ld hl, 0x1234
    ld bc, 0x4321
    ld de, 0x3412
    ld ix, 0x7FFE
.loop:
    call drawStatus
.keyloop:
    call getKey_skipCheck
    cp kPlus
    jp z, .plus
    cp kMinus
    jp z, .minus
    cp kEnter
    ret z
    cp kStat
    jp z, .swapPage4
    cp kMODE
    jp z, .swapPage4_alt
    cp kMath
    jp z, .testFile
    jp .keyloop
.swapPage4:
    setBankA(4)
    jp .loop
.swapPage4_alt:
    ld a, 4
    setBankA
    jp .loop
.plus:
    inc ix
    jp .loop
.minus:
    dec ix
    jp .loop
.testFile:
    ld de, testFile
    call openFileRead
    ;call drawStatus \ call flushKeys_skipCheck \ call waitKey_skipCheck
    ;push de
    ;    call getStreamInfo
    ;pop de
    ;call malloc
    ld ix, 0x8000
    call streamReadToEnd
    call closeStream
    call clearBuffer
    push ix \ pop hl
    ld de, 0
    ld b, 0
    call drawStr
    call fastCopy_skipCheck
    call flushKeys_skipCheck \ call waitKey_skipCheck
    jp boot

zeroResetText:
    .db "\nZero flag reset", 0
zeroSetText:
    .db "\nZero flag set", 0
interruptsDisabledText:
    .db "\nInterrupts disabled", 0
interruptsEnabledText:
    .db "\nInterrupts enabled", 0
checkText:
    .db "Check #", 0
pcRegText:
    .db "\nPC: ", 0
spRegText:
    .db "        SP: ", 0
aRegText:
    .db "\n\nA: ", 0
derIXRegText:
    .db "                    (IX): ", 0
hlRegText:
    .db "\nHL: ", 0
bcRegText:
    .db "        BC: ", 0
deRegText:
    .db "\nDE: ", 0
ixRegText:
    .db "        IX: ", 0
iyRegText:
    .db "\nIY: ", 0
drawStatus:
    push de \ push bc \ push hl \ push af
.loop:
        call clearBuffer
        ld de, 0
        ld b, 0
        ld hl, checkText
        call drawStr
        ld a, (currentContrast)
        call drawHexA

        ld hl, pcRegText
        call drawStr
        inc sp \ inc sp \ inc sp \ inc sp \ inc sp \ inc sp \ inc sp \ inc sp
        pop hl \ push hl
        dec sp \ dec sp \ dec sp \ dec sp \ dec sp \ dec sp \ dec sp \ dec sp
        call drawHexHL

        ld hl, spRegText
        call drawStr
        inc sp \ inc sp \ inc sp \ inc sp \ inc sp \ inc sp \ inc sp \ inc sp
        ld hl, 0 \ add hl, sp
        dec sp \ dec sp \ dec sp \ dec sp \ dec sp \ dec sp \ dec sp \ dec sp
        call drawHexHL

        ld hl, aRegText
        call drawStr
        pop af \ push af
        call drawHexA

        ld hl, derIXRegText
        call drawStr
        ld h, (ix)
        ld l, (ix + 1)
        call drawHexHL

        ld hl, hlRegText
        call drawStr
        inc sp \ inc sp
        pop hl \ push hl
        dec sp \ dec sp
        call drawHexHL

        ld hl, bcRegText
        call drawStr
        inc sp \ inc sp \ inc sp \ inc sp
        pop hl \ push hl
        dec sp \ dec sp \ dec sp \ dec sp
        call drawHexHL

        ld hl, deRegText
        call drawStr
        inc sp \ inc sp \ inc sp \ inc sp \ inc sp \ inc sp
        pop hl \ push hl
        dec sp \ dec sp \ dec sp \ dec sp \ dec sp \ dec sp
        call drawHexHL

        ld hl, ixRegText
        call drawStr
        push ix \ pop hl
        call drawHexHL

        ld hl, iyRegText
        call drawStr
        push iy \ pop hl
        call drawHexHL

        pop af \ push af
        jr z, _
        ld hl, zeroResetText
        jr _
_:      ld hl, zeroSetText
_:      call drawStr

        ld a, i
        jp po, _
        ld hl, interruptsEnabledText
        jr _
_:      ld hl, interruptsDisabledText
_:      call drawStr

        call fastCopy_skipCheck
        call flushKeys_skipCheck
        call waitKey_skipCheck

        cp kYEqu
        jp z, boot
    pop af \ pop hl \ pop bc \ pop de
    ret
