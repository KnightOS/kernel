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
; CSE support temporarily uses the same configuration as 84+ SE support
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
