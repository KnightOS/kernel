; TI-8x constants
.macro define_mask(name, bitN)
    BIT_name .equ bitN \
    name .equ 1 << bitN
.endmacro

; Port numbers and outputs
    PORT_KEYPAD         .equ 1
    
    PORT_CALC_STATUS    .equ 2
    PORT_INT_ACK        .equ 2
        define_mask(CALC_STATUS_BATTERY, 0)
        ; 83+ SE/84+ only
        define_mask(CALC_STATUS_LCDBUSY, 1)
        ; 73/83+ BE only
        define_mask(CALC_STATUS_ISTI73, 1)
        define_mask(CALC_STATUS_FLASHUNLOCKED, 2)
        define_mask(CALC_STATUS_HASUSB, 5)
        define_mask(CALC_STATUS_LINKASSIST_AVAILABLE, 6)
        define_mask(CALC_STATUS_IS83PBE, 7)
    
    PORT_INT_MASK       .equ 3
        define_mask(INT_ON, 0)
        define_mask(INT_TIMER1, 1)
        define_mask(INT_TIMER2, 2)
        define_mask(INT_LINK, 3)
    
    ; read
    PORT_INT_TRIG       .equ 4
        define_mask(INT_TRIG_ON, 0)
        define_mask(INT_TRIG_TIMER1, 1)
        define_mask(INT_TRIG_TIMER2, 2)
        define_mask(INT_TRIG_ON_HELD, 3)
        define_mask(INT_TRIG_LINK, 4)
        ; 83+ SE/84+ only
        define_mask(INT_TRIG_CRYS1, 5)
        define_mask(INT_TRIG_CRYS2, 6)
        define_mask(INT_TRIG_CRYS3, 7)
    ; write
    PORT_MEM_TIMER      .equ 4
        define_mask(MEM_TIMER_MODE1, 0)
        define_mask(MEM_TIMER_SPEED, 1)
        ; 83+ SE/84+ only
        define_mask(MEM_TIMER_BATTERY, 6)
        
        
    PORT_RAM_PAGING     .equ 5
    
    PORT_BANKA          .equ 6
        ; 73/83+ BE only
        define_mask(BANKA_ISRAM_CPU6, 6)
        ; 83+ SE/84+ only
        define_mask(BANKA_ISRAM_CPU15, 7)
    
    PORT_BANKB          .equ 7
        ; 73/83+ BE only
        define_mask(BANKB_ISRAM_CPU6, 6)
        ; 83+ SE/84+ only
        define_mask(BANKB_ISRAM_CPU15, 7)
    
    PORT_MEMA_HIGH      .equ 0x0E
    
    PORT_MEMB_HIGH      .equ 0x0F
    
    PORT_LCD_CMD        .equ 0x10
        ; non-color calcs only
        ; read
        define_mask(LCD_CMD_AUTOINC, 0)
        define_mask(LCD_CMD_AUTOINC_AFFECTSCOL, 1)
        define_mask(LCD_CMD_RESETSTATE, 4)
        define_mask(LCD_CMD_DISPLAYING, 5)
        define_mask(LCD_CMD_8BITS, 6)
        define_mask(LCD_CMD_BUSY, 7)
        ; write
        LCD_CMD_SETOUTPUTMODE               .equ 0
        LCD_CMD_SETDISPLAY                  .equ 2
        LCD_CMD_AUTOINCDEC_SETX             .equ 4
        LCD_CMD_AUTOINCDEC_SETY             .equ 6
        LCD_CMD_POWERSUPPLY_SETENHANCEMENT  .equ 8
        LCD_CMD_MIRRORSCREEN                .equ 0x0C
        LCD_CMD_POWERSUPPLY_SETLEVEL        .equ 0x10
        LCD_CMD_CANCELTESTMODE              .equ 0x18
        LCD_CMD_ENTERTESTMODE               .equ 0x1C
        LCD_CMD_SETCOLUMN                   .equ 0x20
        LCD_CMD_SETZSHIFT                   .equ 0x40
        LCD_CMD_SETROW                      .equ 0x80
        LCD_CMD_SETCONTRAST                 .equ 0xC0
    
    PORT_LCD_DATA       .equ 0x11
    
    PORT_FLASHRWCONTROL .equ 0x14
        define_mask(FLASHRWCONTROL_ENABLEWRITE, 0)
    
    ; 73/83+ BE only
    PORT_FLASHEXCLUSION .equ 0x16
    
    ; 83+ SE/84+ only
    PORT_CPUSPEED       .equ 0x20
        define_mask(CPUSPEED_6MHZ, 0)
        define_mask(CPUSPEED_15MHZ, 1)
        ; there are also 2 and 3, but they should not be used
    
    ; 83+ SE/84+ only
    PORT_FLASHRAMSIZE       .equ 0x21
        define_mask(FLASHRAMSIZE_FLASHCHIP, 0)
        define_mask(FLASHRAMSIZE_RAMCHIP, 4)
    
    ; 83+ SE/84+ only
    PORT_FLASHEXEC_LOWLIMIT .equ 0x22
    
    ; 83+ SE/84+ only
    PORT_FLASHEXEC_UPLIMIT  .equ 0x23
    
    ; 83+ SE/84+ only
    PORT_RAMEXEC_LOWLIMIT .equ 0x25
        
    ; 83+ SE/84+ only
    PORT_RAMEXEC_UPLIMIT .equ 0x26
    
    ; 83+ SE/84+ only
    PORT_LCD_DELAY      .equ 0x2A
        define_mask(LCD_DELAY_FLASH, 0)
        define_mask(LCD_DELAY_RAM, 1)
        define_mask(LCD_DELAY_AMOUNT, 2)
    
    ; 84+ only
    PORT_GPIO_CONFIG    .equ 0x39
    
    ; 84+ only
    PORT_GPIO_RW        .equ 0x3A
        ; color only
        define_mask(GPIO_RW_BACKLIGHT, 5)
    
    ; Clocks are 84+ only
    PORT_CLOCKCONTROL   .equ 0x40
        define_mask(CLOCKCONTROL_ENABLE, 0)
        define_mask(CLOCKCONTROL_COMMAND, 1)
    
    PORT_CLOCKREG1_IN   .equ 0x41
    
    PORT_CLOCKREG2_IN   .equ 0x42
    
    PORT_CLOCKREG3_IN   .equ 0x43
    
    PORT_CLOCKREG4_IN   .equ 0x44
    
    PORT_CLOCKREG1_OUT  .equ 0x45
    
    PORT_CLOCKREG2_OUT  .equ 0x46
    
    PORT_CLOCKREG3_OUT  .equ 0x47
    
    PORT_CLOCKREG4_OUT  .equ 0x48
    
    ; USB is 84+ only
    PORT_USB_INT        .equ 0x55
        define_mask(USB_INT_BUS, 0)
        define_mask(USB_INT_UNKNOWN1, 1)
        define_mask(USB_INT_LINE, 2)
        define_mask(USB_INT_VIEWSCREEN_MISS, 3)
        define_mask(USB_INT_PROTOCOL, 4)
        define_mask(USB_INT_UNKNOWN5, 5)
        define_mask(USB_INT_UNKNOWN6, 6)
        define_mask(USB_INT_UNKNOWN7, 7)
        
    PORT_USB_LINE       .equ 0x56
        define_mask(USB_LINE_DPLOW, 0)
        define_mask(USB_LINE_DPHIGH, 1)
        define_mask(USB_LINE_DMLOW, 2)
        define_mask(USB_LINE_DMHIGH, 3)
        define_mask(USB_LINE_IDLOW, 4)
        define_mask(USB_LINE_IDHIGH, 5)
        ; note the order
        define_mask(USB_LINE_VBUSHIGH, 6)
        define_mask(USB_LINE_VBUSLOW, 7)
        
    PORT_USB_LINE_MASK  .equ 0x57
        define_mask(USB_LINE_MASK_DPLOW, 0)
        define_mask(USB_LINE_MASK_DPHIGH, 1)
        define_mask(USB_LINE_MASK_DMLOW, 2)
        define_mask(USB_LINE_MASK_DMHIGH, 3)
        define_mask(USB_LINE_MASK_IDLOW, 4)
        define_mask(USB_LINE_MASK_IDHIGH, 5)
        ; again, note the order
        define_mask(USB_LINE_MASK_VBUSHIGH, 6)
        define_mask(USB_LINE_MASK_VBUSLOW, 7)
        
    PORT_USB_WRPIPE1    .equ 0x82
    
    PORT_USB_WRPIPE2    .equ 0x83
    
    PORT_USB_RDPIPE1    .equ 0x84
    
    PORT_USB_RDPIPE2    .equ 0x85
    
    PORT_USB_MISC_EVENTS .equ 0x86
    
; LCD registers
; TI-84+ CSE only
    
    LCDREG_DRIVER_OUTPUTCONTROL1    .equ 1
    LCDREG_LCDDRIVING_CONTROL       .equ 2
    LCDREG_ENTRYMODE                .equ 3
    LCDREG_UNKNOWN6                 .equ 6
    LCDREG_DISPCONTROL1             .equ 7
    LCDREG_DISPCONTROL2             .equ 8
    LCDREG_DISPCONTROL3             .equ 9
    LCDREG_DISPCONTROL4             .equ 0x0A
    LCDREG_RGBDISP_INTERFACECONTROL .equ 0x0C
    LCDREG_FRAMEMAKER_POSITION      .equ 0x0D
    LCDREG_POWERCONTROL1            .equ 0x10
    LCDREG_POWERCONTROL2            .equ 0x11
    LCDREG_POWERCONTROL3            .equ 0x12
    LCDREG_POWERCONTROL4            .equ 0x13
    LCDREG_CURSOR_ROW               .equ 0x20
    LCDREG_CURSOR_COLUMN            .equ 0x21
    LCDREG_GRAM                     .equ 0x22
    LCDREG_POWERCONTROL7            .equ 0x29
    LCDREG_FRAMERATE                .equ 0x2B
    LCDREG_GAMMA1                   .equ 0x30
    LCDREG_GAMMA2                   .equ 0x31
    LCDREG_GAMMA3                   .equ 0x32
    LCDREG_GAMMA4                   .equ 0x35
    LCDREG_GAMMA5                   .equ 0x36
    LCDREG_GAMMA6                   .equ 0x37
    LCDREG_GAMMA7                   .equ 0x38
    LCDREG_GAMMA8                   .equ 0x39
    LCDREG_GAMMA9                   .equ 0x3C
    LCDREG_GAMMA10                  .equ 0x3D
    LCDREG_WINDOW_HORIZ_START       .equ 0x50
    LCDREG_WINDOW_HORIZ_END         .equ 0x51
    LCDREG_WINDOW_VERT_START        .equ 0x52
    LCDREG_WINDOW_VERT_END          .equ 0x53
    LCDREG_GATESCANCONTROL          .equ 0x60
    LCDREG_BASEIMAGEDISPLAYCONTROL  .equ 0x61
    LCDREG_VERTSCROLLCONTROL        .equ 0x6A
    LCDREG_PARTIALIMG1_DISPPOS      .equ 0x80
    LCDREG_PANELINTERFACECONTROL1   .equ 0x90
    LCDREG_PANELINTERFACECONTROL2   .equ 0x92
    ; if you were wondering, this is normal
    LCDREG_PANELINTERFACECONTROL4   .equ 0x95
    LCDREG_PANELINTERFACECONTROL5   .equ 0x97
    
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
