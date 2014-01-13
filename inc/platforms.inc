#ifdef TI73
privledgedPage .equ 0x1C
swapSector .equ 0x18
fatStart .equ 0x17
bootPage .equ 0x1F
#endif

#ifdef TI83p
privledgedPage .equ 0x1C
swapSector .equ 0x18
fatStart .equ 0x17
bootPage .equ 0x1F
#endif

#ifdef TI83pSE
#define CPU15
privledgedPage .equ 0x7C
swapSector .equ 0x78
fatStart .equ 0x77
bootPage .equ 0x7F
#endif

#ifdef TI84p
#define CPU15
#define USB
#define CLOCK
privledgedPage .equ 0x3C
swapSector .equ 0x38
fatStart .equ 0x37
bootPage .equ 0x3F
#endif

#ifdef TI84pSE
#define CPU15
#define USB
#define CLOCK
privledgedPage .equ 0x7C
swapSector .equ 0x78
fatStart .equ 0x77
bootPage .equ 0x7F
#endif

#ifdef TI84pCSE
#define CPU15
#define USB
#define CLOCK
#define COLOR
#define FLASH4MB
privledgedPage .equ 0xEC
swapSector .equ 0xE8
fatStart .equ 0xE7
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
        .if page & 0b1000000
            ld a, 1
            out (0x0E), a
            ld a, page & 0b10000000
            out (6), a
        .else
            ld a, 0
            out (0x0E), a
            ld a, page
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
        .if page & 0b1000000
            ld a, 1
            out (0x0F), a
            ld a, page & 0b10000000
            out (7), a
        .else
            ld a, 0
            out (0x0F), a
            ld a, page
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
