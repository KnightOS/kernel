.org 0x4000

    rst 0 ; Crash before runaway code breaks things

unlockFlash:
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
    out (0x14),a
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
    out (0x14),a
    pop af
    ret po
    ei
    ret
