; Describes kernel memory layout

kernelVersion           .equ 0x64

; Modify these values to automatically modify memory layout
maxThreads              .equ 32
maxLibraries            .equ 16
maxSignals              .equ 16
maxFileStreams          .equ 8
maxIOFrames             .equ 8
kernelGarbageSize       .equ 0x200
flashFunctionSize       .equ 0x100
stateMemorySize         .equ 0x100

kernelMem .equ 0x8000

threadTable             .equ kernelMem
threadTableSize         .equ 8 * maxThreads             ; 8 bytes per entry

libraryTable            .equ threadTable + threadTableSize
libraryTableSize        .equ 4 * maxLibraries           ; 4 bytes per entry

signalTable             .equ libraryTable + libraryTableSize
signalTableSize         .equ 4 * maxSignals             ; 4 bytes per entry

fileHandleTable         .equ signalTable + signalTableSize
fileHandleTableSize     .equ 16 * maxFileStreams         ; 16 bytes per entry

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

IOstate                 .equ bcallHook + 2              ; 1 byte
IOIsSending             .equ IOstate + 1                ; 1 byte
currentIOFrame          .equ IOIsSending + 1            ; 1 byte
busyIOFrame             .equ currentIOFrame + 1         ; 1 byte
currentIODataByte       .equ busyIOFrame + 1            ; 1 byte
IODataChecksum          .equ currentIODataByte + 1      ; 1 byte
IOTransferErrored       .equ IODataChecksum + 1         ; 1 byte
willSendNextIOFrame     .equ IOTransferErrored + 1      ; 1 byte
temp_io_var             .equ willSendNextIOFrame + 1    ; 1 byte

; Used for line drawing
; TODO: Phase this out
clip_mask               .equ temp_io_var + 1            ; 1 byte

color_mode              .equ clip_mask + 1              ; 1 byte

random_seed             .equ color_mode + 1             ; 8 bytes

flashFunctions          .equ stateMemory + stateMemorySize
kernelGarbage           .equ flashFunctions + flashFunctionSize

IOFramesQueue           .equ kernelGarbage + kernelGarbageSize
IOFramesQueueSize       .equ 6 * maxIOFrames            ; 6 bytes per entry

userMemory              .equ IOFramesQueue + IOFramesQueueSize
userMemorySize          .equ 0xFFFF - userMemory        ; user memory extends to 0xFFFF
