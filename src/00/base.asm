; Base file for KnightOS kernel
#include "platforms.inc"
#include "defines.inc"
#include "kernelmem.inc"
#include "keys.inc"

#include "header.asm"
#include "boot.asm"
#include "restarts.asm"
#include "interrupt.asm"
#include "panic.asm"

#include "memory.asm"
#include "flash.asm"
#include "filesystem.asm"
#include "filestreams.asm"

#include "thread.asm"
#include "libraries.asm"
#include "signals.asm"
#include "locks.asm"

#include "display.asm"
#include "display-color.asm"
#include "keyboard.asm"

#include "math.asm"
#include "util.asm"

#ifdef TEST
#include "testrunner.asm"
#endif

.echo "Assigned kernel memory:"
.echo "threadTable: 0x{0:X4}" threadTable
.echo "libraryTable: 0x{0:X4}" libraryTable
.echo "signalTable: 0x{0:X4}" signalTable
.echo "fileHandleTable: 0x{0:X4}" fileHandleTable
.echo "stateMemory: 0x{0:X4}" stateMemory
.echo "flashFunctions: 0x{0:X4}" flashFunctions
.echo "kernelGarbage: 0x{0:X4}" kernelGarbage
.echo "userMemory: 0x{0:X4}" userMemory

.echo "Bytes remaining on page 00: {0}" 0x4000-$ ; TODO: Fix this problem in sass
