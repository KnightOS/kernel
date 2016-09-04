#include "platforms.inc"
#include "defines.inc"
#include "keys.inc"
#include "kernelmem.inc"
#include "constants.asm"

#include "00.sym"

.org 0x4000
#include "crypto.asm"
#include "time.asm"
#include "compression.asm"
#include "sort.asm"
#include "graphics.asm"
#include "strings.asm"

.echo "Bytes remaining on page 02: {0}" 0x8000-$
