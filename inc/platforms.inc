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