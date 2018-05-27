#include "constants.asm"
; Dummy boot page to get emulators to boot the OS
    jr _
    .fill 0x0F - $
    .db "Emulated", 0
_:
    in a, (PORT_FLASHRAMSIZE)
    
#ifdef TI84p
    res BIT_FLASHRAMSIZE_FLASHCHIP, a
#else
    set BIT_FLASHRAMSIZE_FLASHCHIP, a
#endif
    
    out (PORT_FLASHRAMSIZE), a
    jp 0x4000
