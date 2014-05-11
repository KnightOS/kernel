; Base file for KnightOS kernel
#define DEBUG

#include "platforms.inc"
#include "defines.inc"
#include "kernelmem.inc"
#include "keys.inc"
#include "constants.asm"

; TODO: Use a proper linker instead of this trash
newline     .equ 0x0001
drawChar    .equ 0x0101
drawCharAND .equ 0x0201
drawCharXOR .equ 0x0301
drawStr     .equ 0x0401
drawStrAND  .equ 0x0501
drawStrXOR  .equ 0x0601
wrapStr     .equ 0x0701
wrapStrAND  .equ 0x0801
wrapStrXOR  .equ 0x0901
drawHexA    .equ 0x0A01
measureChar .equ 0x0B01
measureStr  .equ 0x0C01
drawHexHL   .equ 0x0D01

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

#ifdef DEBUG
#include "debug.asm"
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
