.macro pcall(xxxx)
    rst 0x20
    .dw xxxx
.endmacro
.macro pcall(ff, xxxx)
    jr ff, $+4
    jr $+5
    rst 0x20
    .dw xxxx
.endmacro
#include "platforms.inc"
#include "defines.inc"
#include "keys.inc"

#include "00.inc"

.org 0x4000
#include "text.asm"
#include "font.asm"
