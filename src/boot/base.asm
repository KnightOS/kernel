; Dummy boot page to get emulators to boot the OS
    jr _
    .fill 0x0F - $
    .db "n.nn", 0
_:
#ifdef TI84p
    in a, (0x21)
    res 0, a
    out (0x21), a
#else
    in a, (0x21)
    set 0, a
    out (0x21), a
#endif
    jp 0x4000
