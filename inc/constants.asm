; TI-8x constants

; Port numbers and outputs
    PORT_INT_MASK       .equ 3
        INT_ON          .equ 1
        INT_TIMER1      .equ 2
        INT_TIMER2      .equ 4
        INT_LINK        .equ 8

; System constants
    DEFAULT_STACK_SIZE  .equ 20
    MONO_LCD_WIDTH      .equ 96
    MONO_LCD_HEIGHT     .equ 64
    MONO_BUF_SIZE       .equ 0x300
    COLOR_LCD_WIDTH     .equ 320
    COLOR_LCD_HEIGHT    .equ 240
    LEGACY_BG_COLOR     .equ 0x4108

; KFS stuff
    KFS_BLOCK_SIZE      .equ 0x100

    FILE_HANDLE_SIZE    .equ 16

    FILE_FLAGS          .equ 0
    FILE_BUFFER         .equ 1
    FILE_STREAM         .equ 3
    FILE_SECTION_ID     .equ 4
    FILE_FINAL_LENGTH   .equ 6
    FILE_ENTRY_PAGE     .equ 7
    FILE_ENTRY_PTR      .equ 8
    FILE_WORKING_SIZE   .equ 10
    FILE_WRITE_FLAGS    .equ 13
    FILE_PREV_SECTION   .equ 14
