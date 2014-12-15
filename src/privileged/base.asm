#include "constants.asm"
#include "00.sym"
.org 0x4000

    rst 0 ; Crash before runaway code breaks things

jp _unlockFlash
jp _lockFlash

_unlockFlash:
    ld a,i
    jp pe, _
    ld a, i
_:  push af
    di
    ld a, 1
    nop
    nop
    im 1
    di
    out (PORT_FLASHRWCONTROL), a
    pop af
    ret po
    ei
    ret
    
_lockFlash:
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
    out (PORT_FLASHRWCONTROL), a
    pop af
    ret po
    ei
    ret
