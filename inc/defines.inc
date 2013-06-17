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


; ** SIZES OF KERNEL TABLES **
; Modify individual values as needed, everything else will shift around automatically.
kernelMem .equ 0x8000

threadTable .equ kernelMem
maxThreads .equ 32
threadTableSize .equ 8 * maxThreads                     ; 8 bytes per entry

libraryTable .equ threadTable + threadTableSize
maxLibraries .equ 16
libraryTableSize .equ 4 * maxLibraries                  ; 4 bytes per entry

signalTable .equ libraryTable + libraryTableSize
maxSignals .equ 16
signalTableSize .equ 4 * maxSignals                     ; 4 bytes per entry

fileHandleTable .equ signalTable + signalTableSize
maxFileStreams .equ 16
fileHandleTableSize .equ 8 * maxFileStreams             ; 8 bytes per entry

tablesSize .equ threadTableSize + libraryTableSize + signalTableSize + fileHandleTableSize

; ** MISCELLANEOUS STATE MEMORY ASSIGNMENTS **
; Again, allocate as needed, and increase the size if necessary.

stateMemory .equ kernelMem + tablesSize

currentThreadIndex .equ stateMemory                     ; 1 byte
activeThreads .equ currentThreadIndex + 1               ; 1 byte
loadedLibraries .equ activeThreads + 1                  ; 1 byte
activeSignals .equ loadedLibraries + 1                  ; 1 byte
activeFileStreams .equ activeSignals + 1                ; 1 byte
lastThreadId .equ activeFileStreams + 1                 ; 1 byte

hwLockIO .equ lastThreadId + 1                          ; 1 byte
hwLockLCD .equ hwLockIO + 1                             ; 1 byte
hwLockKeypad .equ hwLockLCD + 1                         ; 1 byte
hwLockUSB .equ hwLockKeypad + 1                         ; 1 byte

; Misc.
currentContrast .equ hwLockUSB + 1                      ; 1 byte
bcallHook .equ  currentContrast + 1                     ; 2 bytes

; Used for line drawing
clip_mask .equ bcallHook + 2                            ; 1 byte

priorityHook .equ clip_mask + 1                         ; 1 byte

stateMemorySize .equ 0x100                              ; Hardcoded constant

; ** KERNEL GARBAGE MEMORY ALLOCATION **
kernelGarbage .equ stateMemory + stateMemorySize
kernelGarbageSize .equ 0x200                            ; Hardcoded constant

; ** USER MEMORY DECLARATION **
userMemory .equ kernelGarbage + kernelGarbageSize
userMemorySize .equ 0xFFFF - userMemory                 ; user memory extends to 0xFFFF

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
