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
errFilesystemFull .equ 11
errNameTooLong .equ 12

#include "kernelmem.inc"

threadRangeMask .equ 0b11111

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

panic_init_not_found .equ 0
panic_no_threads .equ 1
panic_no_active_threads .equ 2
panic_library_not_found .equ 3
