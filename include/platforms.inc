#ifdef TI73
#define LINK_ASSIST_RO
swapSector .equ 0x18
fatStart .equ 0x17
privledgedPage .equ 0x1C
bootPage .equ 0x1F
#endif

#ifdef TI83p
#define LINK_ASSIST_RO
swapSector .equ 0x18
fatStart .equ 0x17
privledgedPage .equ 0x1C
bootPage .equ 0x1F
#endif

#ifdef TI83pSE
#define CPU15
#define CRYSTAL_TIMERS
#define LINK_ASSIST
#define MD5_ACCELERATION
swapSector .equ 0x78
fatStart .equ 0x77
privledgedPage .equ 0x7C
bootPage .equ 0x7F
#endif

#ifdef TI84p
#define CPU15
#define CRYSTAL_TIMERS
#define LINK_ASSIST
#define MD5_ACCELERATION
#define USB
#define CLOCK
swapSector .equ 0x38
fatStart .equ 0x37
privledgedPage .equ 0x3C
bootPage .equ 0x3F
#endif

#ifdef TI84pSE
#define CPU15
#define CRYSTAL_TIMERS
#define LINK_ASSIST
#define MD5_ACCELERATION
#define USB
#define CLOCK
swapSector .equ 0x78
fatStart .equ 0x77
privledgedPage .equ 0x7C
bootPage .equ 0x7F
#endif

#ifdef TI84pCSE
#define CPU15
#define CRYSTAL_TIMERS
#define LINK_ASSIST
#define MD5_ACCELERATION
#define USB
#define CLOCK
#define COLOR
#define FLASH4MB
swapSector .equ 0xF8
fatStart .equ 0xF7
privledgedPage .equ 0xFC
bootPage .equ 0xFF

.macro lcdout(reg, value)
    ld a, reg
    ld hl, value
    call writeLcdRegister
.endmacro
#endif

#ifndef FLASH4MB
    .macro setBankA()
        out (6), a
    .endmacro

    .macro setBankA(page)
        ld a, page
        out (6), a
    .endmacro

    .macro getBankA()
        in a, (6)
    .endmacro

    .macro setBankB()
        out (7), a
    .endmacro

    .macro setBankB(page)
        ld a, page
        out (7), a
    .endmacro

    .macro getBankB()
        in a, (7)
    .endmacro
#else
    .macro setBankA()
        call color_pageBankA
    .endmacro

    .macro setBankA(page)
        .if page & 0x80
            ld a, 1
            out (0x0E), a
            ld a, page & 0x7F
            out (6), a
            ld a, page
        .else
            xor a
            out (0x0E), a
            ld a, page & 0x7F
            out (6), a
        .endif
    .endmacro

    .macro getBankA()
        push bc
            in a, (0x0E)
            ld c, a
            rrc c
            in a, (6)
            or c
        pop bc
    .endmacro

    .macro setBankB()
        call color_pageBankB
    .endmacro

    .macro setBankB(page)
        .if page & 0x80
            ld a, 1
            out (0x0F), a
            ld a, page & 0x7F
            out (7), a
            ld a, page
        .else
            xor a
            out (0x0F), a
            ld a, page & 0x7F
            out (7), a
        .endif
    .endmacro

    .macro getBankB()
        push bc
            in a, (0x0F)
            ld c, a
            rrc c
            in a, (7)
            or c
        pop bc
    .endmacro
#endif
