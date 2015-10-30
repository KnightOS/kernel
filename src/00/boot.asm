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

#ifdef LINK_ASSIST
    ld a, LA_ENABLE_INT_RX | LA_ENABLE_INT_ERROR
    out (PORT_LINK_ASSIST_ENABLE), a
#endif

#ifdef CLOCK
    ld a, 1
    out (PORT_CLOCKCONTROL), a
#endif

    ld hl, 0x8000
    ld (hl), 0
    ld de, 0x8001
    ld bc, 0x7FFF
    ldir

    call unlockFlash
    call unprotectRAM
    call unprotectFlash
    call lockFlash

#ifdef CPU15
    ld a, BIT_CPUSPEED_15MHZ
    out (PORT_CPUSPEED), a
#endif

    ld a, INT_ON | INT_TIMER1 | INT_LINK
    out (PORT_INT_MASK), a

    call formatMem
    call initRandom
    call initFilesystem
    call initMultitasking
    call initDisplay
    call initIO

    ld iy, 0xC000
    call clearBuffer
    ld hl, init
    ld de, 0
    rst 0x20
    .dw draw3x3str
    call fastCopy
    jr $

    ld de, init
    call fileExists
    ld a, panic_init_not_found
    jp nz, panic
    call launchProgram
    ld h, 0
    call setInitialA
    jp contextSwitch_manual

init:
    .db "/bin/init", 0
