nullThread              .equ 0xFF
errOutOfMem             .equ 1
errTooManyThreads       .equ 2
errStreamNotFound       .equ 3
errEndOfStream          .equ 4
errFileNotFound         .equ 5
errTooManyStreams       .equ 6
errNoSuchThread         .equ 7
errTooManyLibraries     .equ 8
errUnsupported          .equ 9
errTooManySignals       .equ 10
errFilesystemFull       .equ 11
errNameTooLong          .equ 12
errAlreadyExists        .equ 13
errNoMagic              .equ 14
errNoHeader             .equ 15
errNoEntryPoint         .equ 15
errKernelMismatch       .equ 15
errMutexNotLocked       .equ 16
errReadOnly             .equ 17
errNotAFile             .equ 18
errNotADirectory        .equ 18

threadRangeMask         .equ 0b11111

; Filesystem stuff
fsEndOfTable            .equ 0xFF
fsFile                  .equ 0x7F
fsDirectory             .equ 0xBF
fsSymLink               .equ 0xDF
fsDeletedFile           .equ 0x00
fsModifiedFile          .equ 0x01
fsDeletedDirectory      .equ 0x02
fsModifiedDirectory     .equ 0x04
fsDeletedSymLink        .equ 0x08

panic_init_not_found    .equ 0
panic_no_threads        .equ 1
panic_no_active_threads .equ 2
panic_library_not_found .equ 3
panic_failed_init       .equ 4

; KEXC headers
; 0x00-0x7F reserved for kernel use
; 0x80-0xFF available for arbituary use
KEXC_HEADER_END         .equ 0x00
KEXC_ENTRY_POINT        .equ 0x01
KEXC_STACK_SIZE         .equ 0x02
KEXC_KERNEL_VER         .equ 0x03
KEXC_THREAD_FLAGS       .equ 0x04
KEXC_NAME               .equ 0x05
KEXC_DESCRIPTION        .equ 0x06
KEXC_RELOCATION_TABLE   .equ 0x07

; Thread flags
THREAD_NON_SUSPENDABLE  .equ 2
;THREAD_IS_SUSPENDED    .equ 4 ; Not a public flag
THREAD_COLOR            .equ 8
