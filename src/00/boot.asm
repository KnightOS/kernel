; Calculator boot-up code
boot:
    di
    jr _
;; shutdown [System]
;;  Shuts off the device.
shutdown:
    ; TODO: Crash detection
_:  di
    
#ifdef CPU15
    ; enable R/W link assist and all interrupt triggering
    ld a, LINKASSIST_INT_ONRECV | LINKASSIST_INT_ONREADY | LINKASSIST_INT_ONERROR
    out (PORT_LINKASSIST_ENABLE), a
#else
    ; enable R link assist as the 73/83+ BE only has read support
    ld a, LINKPORT_ASSIST_ACTIVE
    out (PORT_LINKPORT), a
#endif
    
    ld a, IO_STATE_IDLE
    ld (IOstate), a
    ld hl, 0
    ld (currentIOFrame), hl ; set currentIODataByte at the same time
    
    
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
