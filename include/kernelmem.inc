; Describes kernel memory layout

kernelVersion           .equ 0x64

; Modify these values to automatically modify memory layout
maxThreads              .equ 32
maxLibraries            .equ 16
maxSignals              .equ 16
maxFileStreams          .equ 8
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

; Used for line drawing
; TODO: Phase this out
clip_mask               .equ currentContrast + 2        ; 1 byte

color_mode              .equ clip_mask + 1              ; 1 byte

random_seed             .equ color_mode + 1             ; 8 bytes

; This is incremented on timer 1, which is a little over 100 Hz
kernel_current_time     .equ random_seed + 8            ; 2 bytes

; Link assist
io_header_buffer        .equ kernel_current_time + 2    ; 4 bytes
io_header_ix            .equ io_header_buffer + 4       ; 1 byte
io_last_byte_time       .equ io_header_ix + 1           ; 2 bytes
io_bulk_buffer          .equ io_last_byte_time + 2      ; 2 bytes
io_bulk_buffer_bak      .equ io_bulk_buffer + 2         ; 2 bytes
io_bulk_len             .equ io_bulk_buffer_bak + 2     ; 2 bytes
io_bulk_callback        .equ io_bulk_len + 2            ; 2 bytes
io_bulk_callback_thread .equ io_bulk_callback + 2       ; 1 byte

kbd_scan_len            .equ io_bulk_callback_thread + 1; 1 byte
kbd_scan_queue          .equ kbd_scan_len + 1           ; 16 bytes

io_tx_header            .equ kbd_scan_queue + 16        ; 4 bytes
io_tx_header_ix         .equ io_tx_header + 4           ; 1 byte
io_send_queue           .equ io_tx_header_ix + 1        ; 2 bytes
io_send_queue_bak       .equ io_send_queue + 2          ; 2 bytes
io_send_remain          .equ io_send_queue_bak + 2      ; 2 bytes
io_send_callback        .equ io_send_remain + 2         ; 2 bytes
io_send_callback_thread .equ io_send_callback + 2       ; 1 bytes
io_header_handlers      .equ io_send_callback_thread + 1; 2 bytes
io_checksum             .equ io_header_handlers + 2     ; 2 bytes

stateUsed               .equ io_header_handlers + 2 - stateMemory

flashFunctions          .equ stateMemory + stateMemorySize
kernelGarbage           .equ flashFunctions + flashFunctionSize

userMemory              .equ kernelGarbage + kernelGarbageSize
userMemorySize          .equ 0xFFFF - userMemory        ; user memory extends to 0xFFFF
