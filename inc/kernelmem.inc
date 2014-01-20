; Describes kernel memory layout

; Modify these values to automatically modify memory layout
maxThreads              .equ 32
maxLibraries            .equ 16
maxSignals              .equ 16
maxFileStreams          .equ 16
kernelGarbageSize       .equ 0x200
stateMemorySize         .equ 0x100

kernelMem .equ 0x8000

threadTable             .equ kernelMem
threadTableSize         .equ 8 * maxThreads             ; 8 bytes per entry

libraryTable            .equ threadTable + threadTableSize
libraryTableSize        .equ 4 * maxLibraries           ; 4 bytes per entry

signalTable             .equ libraryTable + libraryTableSize
signalTableSize         .equ 4 * maxSignals             ; 4 bytes per entry

fileHandleTable         .equ signalTable + signalTableSize
fileHandleTableSize     .equ 8 * maxFileStreams         ; 8 bytes per entry

kernelTablesSize        .equ threadTableSize + libraryTableSize + signalTableSize + fileHandleTableSize

stateMemory             .equ kernelMem + kernelTablesSize

currentThreadIndex      .equ stateMemory                ; 1 byte
activeThreads           .equ currentThreadIndex + 1     ; 1 byte
loadedLibraries         .equ activeThreads + 1          ; 1 byte
activeSignals           .equ loadedLibraries + 1        ; 1 byte
activeFileStreams       .equ activeSignals + 1          ; 1 byte
lastThreadId            .equ activeFileStreams + 1      ; 1 byte

hwLockIO                .equ lastThreadId + 1           ; 1 byte
hwLockLCD               .equ hwLockIO + 1               ; 1 byte
hwLockKeypad            .equ hwLockLCD + 1              ; 1 byte
hwLockUSB               .equ hwLockKeypad + 1           ; 1 byte

; Misc.
currentContrast         .equ hwLockUSB + 1              ; 1 byte
bcallHook               .equ currentContrast + 1        ; 2 bytes

; Used for line drawing
; TODO: Phase this out
clip_mask               .equ currentContrast + 2        ; 1 byte

kernelGarbage           .equ stateMemory + stateMemorySize

userMemory              .equ kernelGarbage + kernelGarbageSize
userMemorySize          .equ 0xFFFF - userMemory        ; user memory extends to 0xFFFF
