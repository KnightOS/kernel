#include "constants.asm"
.org 0x4000

    rst 0 ; Crash before runaway code breaks things

unlockFlash:
    ld a,i
    jp pe, _
    ld a, i
_:  push af
    di
    ld a, 1 + FLASHRWCONTROL_ENABLEWRITE
    nop
    nop
    im 1
    di
    out (PORT_FLASHRWCONTROL),a
    pop af
    ret po
    ei
    ret
    
lockFlash:
    ld a,i
    jp pe, _
    ld a, i
_:  push af
    di
    xor a
    nop
    nop
    im 1
    di
    out (PORT_FLASHRWCONTROL),a
    pop af
    ret po
    ei
    ret
