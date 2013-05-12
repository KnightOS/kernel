nullThread .equ 0xFF
errOutOfMem .equ 1
errTooManyThreads .equ 2
errStreamNotFound .equ 3
errEndOfStream .equ 4
errFileNotFound .equ 5
errTooManyStreams .equ 6
errNoSuchThread .equ 7
errTooManyLibraries .equ 8
errUnsupported .equ 9
errTooManySignals .equ 10

kernelMem .equ 0x8000
kernelGarbage .equ 0x8100
kernelGarbageSize .equ 0x100
userMemory .equ kernelGarbage + kernelGarbageSize

threadTable .equ kernelMem
libraryTable .equ 0x8050
threadTableSize .equ libraryTable - threadTable ; TODO: More of these? Less?
signalTable .equ 0x8078
fileHandleTable .equ 0x808C
fileHandleTableSize .equ 0x28

currentThreadIndex .equ 0x80B4
activeThreads .equ 0x80B5
loadedLibraries .equ 0x80B6
activeSignals .equ 0x80B7
activeFileStreams .equ 0x80B8
lastThreadId .equ 0x80B9

hwLockIO .equ 0x80BA
hwLockLCD .equ 0x80BB
hwLockKeypad .equ 0x80BC
hwLockUSB .equ 0x80BD

; Misc.
currentContrast .equ 0x80BE
bcallHook .equ  0x80BF ; 2 bytes

; Used for line drawing
clip_mask .equ 0x80C1

priorityHook .equ 0x80C2

maxThreads .equ 10
maxFileStreams .equ 5
maxLibraries .equ 10
maxSignals .equ 5 ; Consider increasing this

threadRangeMask .equ 0x3F

; Filesystem stuff
fsEndOfTable .equ 0xFF
fsFile .equ 0x7F
fsDirectory .equ 0xBF
fsSymLink .equ 0xDF
fsDeletedFile .equ 0x00
fsModifiedFile .equ 0x01
fsDeletedDirectory .equ 0x02
fsModifiedDirectory .equ 0x04
fsDeletedSymLink .equ 0x08
s_readable .equ 0
s_writable .equ 0b10000000
