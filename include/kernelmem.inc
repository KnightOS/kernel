; Describes kernel memory layout

kernel_version           .equ 0x64

; Modify these values to automatically modify memory layout
max_threads              .equ 32
max_libraries            .equ 16
max_signals              .equ 16
max_file_streams          .equ 8
kernel_garbage_size       .equ 0x200
flash_functions_size       .equ 0x100
state_memory_size         .equ 0x100

kernel_mem .equ 0x8000

thread_table             .equ kernel_mem
thread_table_size         .equ 8 * max_threads             ; 8 bytes per entry

library_table            .equ thread_table + thread_table_size
library_table_size        .equ 4 * max_libraries           ; 4 bytes per entry

signal_table             .equ library_table + library_table_size
signal_table_size         .equ 4 * max_signals             ; 4 bytes per entry

file_handle_table         .equ signal_table + signal_table_size
file_handle_table_size     .equ 16 * max_file_streams         ; 16 bytes per entry

kernel_tables_size        .equ thread_table_size + library_table_size + signal_table_size + file_handle_table_size

state_memory             .equ kernel_mem + kernel_tables_size

current_thread_index      .equ state_memory                ; 1 byte
active_threads           .equ current_thread_index + 1     ; 1 byte
loaded_libraries         .equ active_threads + 1          ; 1 byte
active_signals           .equ loaded_libraries + 1        ; 1 byte
active_file_streams       .equ active_signals + 1          ; 1 byte
last_thread_id            .equ active_file_streams + 1      ; 1 byte

hw_lock_io                .equ last_thread_id + 1           ; 1 byte
hw_lock_lcd               .equ hw_lock_io + 1               ; 1 byte
hw_lock_keypad            .equ hw_lock_lcd + 1              ; 1 byte
hw_lock_usb               .equ hw_lock_keypad + 1           ; 1 byte

; Misc.
current_contrast         .equ hw_lock_usb + 1              ; 1 byte
bcall_hook               .equ current_contrast + 1        ; 2 bytes

; Used for line drawing
; TODO: Phase this out
clip_mask               .equ current_contrast + 2        ; 1 byte

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

state_used               .equ io_header_handlers + 2 - state_memory

flash_functions          .equ state_memory + state_memory_size
kernel_garbage           .equ flash_functions + flash_functions_size

heap              .equ kernel_garbage + kernel_garbage_size
heap_size          .equ 0xFFFF - heap        ; heap extends to 0xFFFF
