; Calculator boot-up code
boot:
    di
    jr _
;; shutdown [System]
;;  Shuts off the device.
shutdown:
    ; TODO: Crash detection
_:  di

    ld a, 3 << MEM_TIMER_SPEED
    out (PORT_MEM_TIMER), a ; Memory mode 0

#ifdef FLASH4MB
    xor a
    out (PORT_MEMA_HIGH), a
    out (PORT_MEMB_HIGH), a
#endif

#ifdef CPU15
    ; Set memory mapping
    ; Bank 0: Flash Page 00
    ; Bank 1: Flash Page *
    ; Bank 2: RAM Page 01
    ; Bank 3: RAM Page 00 ; In this order for consistency with TI-83+ and TI-73 mapping
    ld a, 1 | BANKB_ISRAM_CPU15
    out (PORT_BANKB), a
#else
    ; Set memory mapping
    ; Bank 0: Flash Page 00
    ; Bank 1: Flash Page *
    ; Bank 2: RAM Page 01
    ; Bank 3: RAM Page 00
    ld a, 1 | BANKB_ISRAM_CPU6
    out (PORT_BANKB), a
#endif

    ld sp, kernelGarbage + kernelGarbageSize

    call suspendDevice
;; reboot [System]
;;  Restarts the device.
reboot:
    di

    ld sp, kernelGarbage + kernelGarbageSize

#ifdef FLASH4MB
    xor a
    out (PORT_MEMA_HIGH), a
    out (PORT_MEMB_HIGH), a
#endif
    ; Re-map memory
    ld a, 3 << MEM_TIMER_SPEED
    out (PORT_MEM_TIMER), a
#ifdef CPU15
    ld a, 1 | BANKB_ISRAM_CPU15
    out (PORT_BANKB), a
#else
    ld a, 1 | BANKB_ISRAM_CPU6
    out (PORT_BANKB), a
#endif

    ; Manipulate protection states
#ifdef CPU15 ; TI-83+ SE, TI-84+, TI-84+ SE, TI-84+ CSE
    call unlockFlash
        ; Remove RAM Execution Protection
        xor a
        out (PORT_RAMEXEC_LOWLIMIT), a ; RAM Lower Limit ; out (25), 0
        dec a
        out (PORT_RAMEXEC_UPLIMIT), a ; RAM Upper Limit ; out (26), $FF

        ; Remove Flash Execution Protection
        out (PORT_FLASHEXEC_LOWLIMIT), a ; Flash Lower Limit ; out (22), $FF
        out (PORT_FLASHEXEC_UPLIMIT), a ; Flash Upper Limit ; out (23), $FF
    call lockFlash

    ; Set CPU speed to 15 MHz
    ld a, BIT_CPUSPEED_15MHZ
    out (PORT_CPUSPEED), a

#else ; TI-73, TI-83+
    #ifndef TI73 ; RAM does not have protection on the TI-73

    ; Remove RAM/Flash protection
    call unlockFlash
        xor a
        out (PORT_RAM_PAGING), a
        out (PORT_FLASHEXCLUSION), a

        ld a, 0b000000001
        out (PORT_RAM_PAGING), a
        xor a
        out (PORT_FLASHEXCLUSION), a

        ld a, 0b000000010
        out (PORT_RAM_PAGING), a
        xor a
        out (PORT_FLASHEXCLUSION), a

        ld a, 0b000000111
        out (PORT_RAM_PAGING), a
        xor a
        out (PORT_FLASHEXCLUSION), a
    call lockFlash
    #endif
#endif

    ; Set interrupt mode
    ld a, INT_ON | INT_TIMER1 | INT_LINK
    out (PORT_INT_MASK), a
    ; Set timer frequency (TODO)

    ; Clear RAM
    ld hl, 0x8000
    ld (hl), 0
    ld de, 0x8001
    ld bc, 0x7FFF
    ldir

    call formatMem

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
    out (PORT_GPIO_CONFIG), a
    ld a, 1
    ld (color_mode), a
#else
    ; Initialize LCD
    ld a, 1 + LCD_CMD_AUTOINCDEC_SETX
    call lcdDelay
    out (PORT_LCD_CMD), a ; X-Increment Mode

    ld a, 1 + LCD_CMD_SETOUTPUTMODE
    call lcdDelay
    out (PORT_LCD_CMD), a ; 8-bit mode

    ld a, 1 + LCD_CMD_SETDISPLAY
    call lcdDelay
    out (PORT_LCD_CMD), a ; Enable screen

    ld a, 7 + LCD_CMD_POWERSUPPLY_SETLEVEL ; versus +3? TIOS uses +7, and that's the only value that works (the datasheet says go with +3)
    call lcdDelay
    out (PORT_LCD_CMD), a ; Op-amp control (OPA1) set to max (with DB1 set for some reason)

    ld a, 3 + LCD_CMD_POWERSUPPLY_SETENHANCEMENT ; B
    call lcdDelay
    out (PORT_LCD_CMD), a ; Op-amp control (OPA2) set to max

    ; Different amounts of contrast look better on different models
    #ifdef USB
        ld a, 0x2F + LCD_CMD_SETCONTRAST
    #else
        #ifdef TI73
            ld a, 0x3B + LCD_CMD_SETCONTRAST
        #else
            ld a, 0x34 + LCD_CMD_SETCONTRAST
        #endif
    #endif

    ld (currentContrast), a
    call lcdDelay
    out (PORT_LCD_CMD), a ; Contrast
#endif

    call test
    
    ld de, bootFile
    call fileExists
    ld a, panic_init_not_found
    jp nz, panic
    call launchProgram
    ld h, 0
    call setInitialA

    jp contextSwitch_manual

test:
    ld de, testFile
    call fileExists
    ret z
    call openFileWrite
    ; Writing file manually because stream write functions aren't implemented yet
    call getStreamBuffer
    push de
        push hl \ pop de
        ld hl, testString
        ld bc, 0x100
        ldir
    pop de
    call getStreamEntry
    res 0, (ix + FILE_WRITE_FLAGS) ; Mark as not flushed
    xor a
    ld (ix + FILE_STREAM), a ; Set stream pointer to start of next block
    ld bc, testStringEnd - testString
    ld (ix + FILE_WORKING_SIZE), c
    ld (ix + FILE_WORKING_SIZE + 1), b
    ld (ix + FILE_WORKING_SIZE + 2), a
    call advanceBlock
    ; Write next block
    call getStreamBuffer
    push de
        push hl \ pop de
        ld hl, testString_block2
        ld bc, testStringEnd - testString_block2
        ldir
    pop de
    call getStreamEntry
    res 0, (ix + FILE_WRITE_FLAGS) ; Not flushed
    ld a, testStringEnd - testString_block2
    ld (ix + FILE_STREAM), a
    call flush
    call closeStream
    ret

bootFile:
    .db "/bin/init", 0
castle:
    .db "/bin/castle", 0
testFile:
    .db "/var/test", 0
testString:
    .db "This file is over 256 bytes. Ramble ramble ramble ramble ramble ramble ramble ramble ramble ramble blah blah blah blah foo foo foo foo bar bar bar bar bar test test test test test test who knew it was so difficult to type 256 bytes worth of junk abcdefgh\n\n"
testString_block2:
    .db "256 bytes!"
testStringEnd:
