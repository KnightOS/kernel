; Base file for KnightOS kernel

#include "defines.inc"

#include "header.asm"
#include "boot.asm"
#include "restarts.asm"
#include "interrupt.asm"
#include "errors.asm"

#include "memory.asm"
#include "flash.asm"
#include "filesystem.asm"
#include "filestreams.asm"

#include "thread.asm"
#include "libraries.asm"
#include "signals.asm"
#include "locks.asm"

#include "display.asm"
#include "text.asm"
#include "keyboard.asm"
#include "time.asm"

#include "util.asm"

#ifdef TEST
#include "testrunner.asm"
#endif

.echo "Bytes remaining on page 00: " 0x4000-$ ; TODO: Fix this problem in sass