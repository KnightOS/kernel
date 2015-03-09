#ifndef COLOR

setLegacyLcdMode:
    ret

checkLegacyLcdMode:
    cp a
    ret
    
resetLegacyLcdMode:
clearColorLcd:
colorLcdOn:
colorLcdOff:
colorLcdWait:
readLcdRegister:
writeLcdRegister:
clipColorRectangle:
setLcdWindow:
fullScreenWindow:
    or 1
    ld a, errUnsupported
    ret

colorSupported:
    or 1
    ld a, errUnsupported
    ret

#else
; Color screen is 320x240

initDisplay:
    ; Set GPIO config
    ld a, 0xE0
    out (PORT_GPIO_CONFIG), a
    ld a, 1
    ld (color_mode), a
    ret

;; colorSupported [Color]
;;  Sets Z if color is supported on this device.
;; Outputs:
;;  A: errUnsupported if color is unsupported
;;  Z: Set if supported, reset if unsupported
colorSupported:
    cp a
    ret

;; writeLcdRegister [Color]
;;  Writes a 16-bit value to a color LCD register
;; Inputs:
;;  A: Register
;;  HL: Value
;; Comments:
;;  Destroys C
writeLcdRegister:
    out (PORT_LCD_CMD), a \ out (PORT_LCD_CMD), a
    ld c, PORT_LCD_DATA
    out (c), h
    out (c), l
    ret

;; setLcdWindow [Color]
;;  Sets the LCD's clipping window. Values are inclusive.
;; Inputs:
;;  HL: left border
;;  DE: right border
;;  B: top border
;;  C: bottom border 
;; Notes:
;;  Destroys C
setLcdWindow:
    push af \ push hl
        push bc
            ld a, LCDREG_WINDOW_VERT_START
            call writeLcdRegister
            inc a
            out (PORT_LCD_CMD), a
            out (PORT_LCD_CMD), a
            out (c), d
            out (c), e
        pop bc
        ld a, LCDREG_WINDOW_HORIZ_START
        ld h, 0
        ld l, c
        ld c, PORT_LCD_DATA
        out (PORT_LCD_CMD), a
        out (PORT_LCD_CMD), a
        out (c), h
        out (c), b
        inc a
        out (PORT_LCD_CMD), a
        out (PORT_LCD_CMD), a
        out (c), h
        out (c), l
    pop hl \ pop af
    ret
    
;; fullScreenWindow [Color]
;;  Sets the clipping window to fit the LCD screen in color mode.
fullScreenWindow:
    push hl
    push bc
    push af
        lcdout(LCDREG_WINDOW_HORIZ_START, 0)
        lcdout(LCDREG_WINDOW_HORIZ_END, 239)
        lcdout(LCDREG_WINDOW_VERT_END, 319)
    pop af
    pop bc
    pop hl
    ret

;; readLcdRegister [Color]
;;  Reads a 16-bit value from a color LCD register
;; Inputs:
;;  A: Register
;; Outputs:
;;  HL: Value
;; Comments:
;;  Destroys C
readLcdRegister:
    out (PORT_LCD_CMD), a \ out (PORT_LCD_CMD), a
    ld c, PORT_LCD_DATA
    in h, (c)
    in l, (c)
    ret

colorLcdWait:
    ld b, 0xAF
    ld c, 0xFF
    ld hl, 0x8000
.loop:
    ld a, (hl)
    ld (hl), a
    dec bc
    ld a, c
    or b
    jp nz, .loop
    ret

;; colorLcdOn [Color]
;;  Initializes and turns on the color LCD in color mode.
colorLcdOn:
    ; TODO: Optimize this, it could be a lot faster
    ld a, BIT_LCD_DELAY_FLASH | (3 << BIT_LCD_DELAY_AMOUNT)
    out (PORT_LCD_DELAY), a ; LCD delay
    lcdout(LCDREG_DISPCONTROL1, 0x0000) ; Reset Disp.Ctrl.1: LCD scanning, command processing OFF
    lcdout(LCDREG_UNKNOWN6, 0x0000)
    ;lcdout(LCDREG_POWERCONTROL1, 0x07F1) ; Reset Pwr.Ctrl.1: Start RC oscillator, set voltages
    lcdout(LCDREG_POWERCONTROL2, 0x0007) ; Pwr.Ctrl.2: Configure voltages
    lcdout(LCDREG_POWERCONTROL3, 0x008C) ; Pwr.Ctrl.3: More voltages
    lcdout(LCDREG_POWERCONTROL4, 0x1800) ; Pwr.Ctrl.4: Take a wild guess
    lcdout(LCDREG_POWERCONTROL7, 0x0030) ; Pwr.Ctrl.7: I'm not an LCD engineer, don't ask me.
    call colorLcdWait
    lcdout(LCDREG_POWERCONTROL1, 0x0190) ; Init Pwr.Ctrl.1: Exit standby, fiddle with voltages, enable
    lcdout(LCDREG_POWERCONTROL2, 0x0227) ; Pwr.Ctrl.2: Configure voltages
    lcdout(LCDREG_UNKNOWN6, 0x0001)
    call colorLcdWait
    call colorLcdWait
    lcdout(LCDREG_DRIVER_OUTPUTCONTROL1, 0x0000) ; Reset Out.Ctrl.1: Ensure scan directions are not reversed
    lcdout(LCDREG_LCDDRIVING_CONTROL, 0x0200) ; LCD Driving Control: Sets inversion mode=line inversion and disables it
    lcdout(LCDREG_ENTRYMODE, 0x10b8) ; Init. Entry Mode: Cursor moves up/down, down, left, disable
    lcdout(LCDREG_DISPCONTROL2, 0x0202) ; Set front & back porches: 2 blank lines top & bottom
    lcdout(LCDREG_DISPCONTROL3, 0x0000) ; Reset Disp.Ctrl.3: Resets scanning stuff and off-screen voltage
    lcdout(LCDREG_DISPCONTROL4, 0x0000) ; Disp.Ctrl.4: No FMARK
    lcdout(LCDREG_RGBDISP_INTERFACECONTROL, 0x0000) ; RGB Disp.: Off
    lcdout(LCDREG_FRAMEMAKER_POSITION, 0x0000) ; FMARK position: Off
    lcdout(LCDREG_GATESCANCONTROL, 0x2700) ; Driver Output Ctrl. 2
    lcdout(LCDREG_BASEIMAGEDISPLAYCONTROL, 0x0001) ; Base Image Display Ctrl: Use color inversion, no vertical scroll, reset voltage in non-display level
    lcdout(LCDREG_VERTSCROLLCONTROL, 0x0000) ; Reset Vertical Scroll Ctrl.
    lcdout(LCDREG_PANELINTERFACECONTROL1, 0x0010)
    lcdout(LCDREG_PANELINTERFACECONTROL2, 0x0600)
    lcdout(LCDREG_PANELINTERFACECONTROL4, 0x0200)
    lcdout(LCDREG_PANELINTERFACECONTROL5, 0x0c00)
    lcdout(LCDREG_GAMMA1, 0x0000) ; Gamma Control 1
    lcdout(LCDREG_GAMMA2, 0x0305) ; Gamma Control 2
    lcdout(LCDREG_GAMMA3, 0x0002) ; Gamma Control 3
    lcdout(LCDREG_GAMMA4, 0x0301) ; Gamma Control 4
    lcdout(LCDREG_GAMMA5, 0x0004) ; Gamma Control 5
    lcdout(LCDREG_GAMMA6, 0x0507) ; Gamma Control 6
    lcdout(LCDREG_GAMMA7, 0x0204) ; Gamma Control 7
    lcdout(LCDREG_GAMMA8, 0x0707) ; Gamma Control 8
    lcdout(LCDREG_GAMMA9, 0x0103) ; Gamma Control 9
    lcdout(LCDREG_GAMMA10, 0x0004) ; Gamma Control 10
    lcdout(LCDREG_WINDOW_HORIZ_START, 0x0000) ; Horiz.Win.Start: 0
    lcdout(LCDREG_WINDOW_HORIZ_END, 0x00EF) ; Horiz.Win.End: 239 = 240-1
    lcdout(LCDREG_WINDOW_VERT_START, 0x0000) ; Vert.Win.Start: 0
    lcdout(LCDREG_WINDOW_VERT_END, 0x013F) ; Vert.Win.End: 319 = 320-1
    lcdout(LCDREG_FRAMERATE, 0x000B) ; Set frame rate to 70
    lcdout(LCDREG_POWERCONTROL1, 0x1190) ; Init Pwr.Ctrl.1: Exit standby, fiddle with voltages, enable
    lcdout(LCDREG_DISPCONTROL1, 0x0001) ; Reset Disp.Ctrl.1: LCD scanning, command processing OFF
    call colorLcdWait
    call colorLcdWait
    lcdout(LCDREG_DISPCONTROL1, 0x0023) ; Reset Disp.Ctrl.1: LCD scanning, command processing OFF
    call colorLcdWait
    call colorLcdWait
    lcdout(LCDREG_DISPCONTROL1, 0x0133) ; Disp.Ctrl.1: LCD scan & light on, ready to enter standby
    ; Turn on backlight
    in a, (PORT_GPIO_RW)
    set BIT_GPIO_RW_BACKLIGHT, a
    out (PORT_GPIO_RW), a
    xor a
    ; TODO: Remember and restore backlight brightness
    lcdout(LCDREG_ENTRYMODE, 0x10B8) ; Entry mode the way we want it
    ret

;; colorLcdOff [Color]
;;  Turns off the color LCD and backlight.
colorLcdOff:
    lcdout(LCDREG_DISPCONTROL1, 0x00)
    call colorLcdWait
    lcdout(LCDREG_POWERCONTROL1, 0x07F0)
    call colorLcdWait
    lcdout(LCDREG_POWERCONTROL1, 0x07F1)
    ; Turn off backlight
    in a, (PORT_GPIO_RW)
    res BIT_GPIO_RW_BACKLIGHT, a
    out (PORT_GPIO_RW), a
    ret

fastCopy: ; Draws a 96x64 monochrome buffer on a color screen
    call hasLcdLock
    ret nz
fastCopy_skipCheck:
    push hl \ push bc \ push de \ push af
        push iy \ pop hl
        ; Draws a 96x64 monochrome LCD buffer (legacy buffer) to the color LCD
        ld bc, 64 << 8 | PORT_LCD_DATA ; 64 rows in b, and the data port in c
        ld de, ((240 - 128) / 2) << 8 | 0 ; Top of the current window in d, 0xFF in e
.loop:
        ; Set cursor column to 0
        ld a, LCDREG_CURSOR_COLUMN
        out (PORT_LCD_CMD), a \ out (PORT_LCD_CMD), a
        dec a ; ld a, 32
        out (c), a \ out (c), a
        ; Set window top and cursor row to d
        ;ld a, 0x20 ; (aka 32)
        out (PORT_LCD_CMD), a \ out (PORT_LCD_CMD), a
        out (c), e \ out (c), d ; Cursor row
        ld a, LCDREG_WINDOW_HORIZ_START
        out (PORT_LCD_CMD), a \ out (PORT_LCD_CMD), a
        out (c), e \ out (c), d ; Window top
        ; Window bottom to d + 1
        inc d \ inc a
        out (PORT_LCD_CMD), a \ out (PORT_LCD_CMD), a
        out (c), e \ out (c), d
        ; Select data register
        ld a, LCDREG_GRAM
        out (PORT_LCD_CMD), a \ out (PORT_LCD_CMD), a
        ; Draw row
        push de
        push bc
            ld d, 0xFF
            ld b, 12
.innerLoop:
            ld a, (hl)
            inc hl

            rla
            jr nc, _ ; Bit 7
            out (c), e \ out (c), e ; Black
            out (c), e \ out (c), e
            jp ++_
_:          out (c), d \ out (c), d ; White
            out (c), d \ out (c), d
_:          rla
            jr nc, _ ; Bit 6
            out (c), e \ out (c), e
            out (c), e \ out (c), e
            jp ++_
_:          out (c), d \ out (c), d
            out (c), d \ out (c), d
_:          rla
            jr nc, _ ; Bit 5
            out (c), e \ out (c), e
            out (c), e \ out (c), e
            jp ++_
_:          out (c), d \ out (c), d
            out (c), d \ out (c), d
_:          rla
            jr nc, _ ; Bit 4
            out (c), e \ out (c), e
            out (c), e \ out (c), e
            jp ++_
_:          out (c), d \ out (c), d
            out (c), d \ out (c), d
_:          rla
            jr nc, _ ; Bit 3
            out (c), e \ out (c), e
            out (c), e \ out (c), e
            jp ++_
_:          out (c), d \ out (c), d
            out (c), d \ out (c), d
_:          rla
            jr nc, _ ; Bit 2
            out (c), e \ out (c), e
            out (c), e \ out (c), e
            jp ++_
_:          out (c), d \ out (c), d
            out (c), d \ out (c), d
_:          rla
            jr nc, _ ; Bit 1
            out (c), e \ out (c), e
            out (c), e \ out (c), e
            jp ++_
_:          out (c), d \ out (c), d
            out (c), d \ out (c), d
_:          rla
            jr nc, _ ; Bit 0
            out (c), e \ out (c), e
            out (c), e \ out (c), e
            jp ++_
_:          out (c), d \ out (c), d
            out (c), d \ out (c), d
_:
            dec b
            jp nz, .innerLoop
        pop bc
        pop de
        inc d
        dec b
        jp nz, .loop
    pop af \ pop de \ pop bc \ pop hl
    ret

;; clearColorLcd [Color]
;;  Sets all pixels on the LCD to a specified color in color mode.
;; Inputs:
;;  IY: Color in 0bRRRRRGGGGGGBBBBB format
;; Notes:
;;  Overwrites the current clipping window.
clearColorLcd:
    push af
    push hl
    push bc
    push de
        push iy \ pop de
        ; Set window
        ld a, LCDREG_WINDOW_HORIZ_START
        ld hl, 0
        call writeLcdRegister
        inc a \ ld hl, 239
        call writeLcdRegister
        inc a \ ld hl, 0
        call writeLcdRegister
        inc a \ ld hl, 319
        call writeLcdRegister
        ; Set cursor
        ld a, LCDREG_CURSOR_ROW
        ld hl, 0
        call writeLcdRegister
        inc a
        call writeLcdRegister
        inc a
        ; Select GRAM
        out (PORT_LCD_CMD), a \ out (PORT_LCD_CMD), a
        ld c, PORT_LCD_DATA
        ld h, 240
.outerLoop:
        ld b, 40
.innerLoop:
        ; 8 pixels per iteration
        ld a, 0b10100101
        out (c), d
        ld a, 0b00110100
        out (c), e
        ld a, 0b10100101
        out (c), d
        ld a, 0b00110100
        out (c), e
        ld a, 0b10100101
        out (c), d
        ld a, 0b00110100
        out (c), e
        ld a, 0b10100101
        out (c), d
        ld a, 0b00110100
        out (c), e
        ld a, 0b10100101
        out (c), d
        ld a, 0b00110100
        out (c), e
        ld a, 0b10100101
        out (c), d
        ld a, 0b00110100
        out (c), e
        ld a, 0b10100101
        out (c), d
        ld a, 0b00110100
        out (c), e
        ld a, 0b10100101
        out (c), d
        ld a, 0b00110100
        out (c), e
        djnz .innerLoop
        dec h
        jr nz, .outerLoop
    pop de
    pop bc
    pop hl
    pop af
    cp a
    ret

;; setLegacyLcdMode [Color]
;;  Sets the LCD to legacy mode.
;; Notes:
;;  Legacy mode simulates a 96x64 monochrome screen with the help of [[fastCopy]]. Color
;;  graphics are not advised in legacy mode.
setLegacyLcdMode:
    push af
        ld a, (color_mode)
        or a
        jr nz, _
    pop af
    ret
_:      xor a
        ld (color_mode), a
    pop af
    push hl
    push af
    ld a, i
    di
    push af
        call getCurrentThreadId
        call getThreadEntry
        ld a, 5
        add l, a
        ld l, a
        jr nc, _
        inc h
_:      res 3, (hl)
    pop af
    jp po, _
    ei
_:  pop af
    pop hl
setLegacyLcdMode_boot:
    push iy
        ld iy, 0x4108
        call clearColorLcd
    pop iy
    push af
    push bc
    push hl
        ; Set up partial images
        ld a, LCDREG_PARTIALIMG1_DISPPOS
        ld hl, 0
        call writeLcdRegister
        inc a
        ; ld hl, 0
        call writeLcdRegister
        inc a
        ld hl, 159
        call writeLcdRegister
        inc a
        ld hl, 160
        call writeLcdRegister
        inc a
        ld hl, 0
        call writeLcdRegister
        inc a
        ld hl, 159 ; 95
        call writeLcdRegister
        ; Set BASEE = 0, both partial images = 1
        ld a, LCDREG_DISPCONTROL1
        out (PORT_LCD_CMD), a \ out (PORT_LCD_CMD), a
        ld c, PORT_LCD_DATA
        in a, (PORT_LCD_DATA) \ in l, (c)
        or  0b00110000 ; Partial images
        and 0b11111110 ; BASEE
        out (PORT_LCD_DATA), a
        out (c), l
        ; Set interlacing on
        lcdout(LCDREG_DRIVER_OUTPUTCONTROL1, 0b0000010000000000)
        ; Set window rows
        lcdout(LCDREG_WINDOW_VERT_START, (160 - 96) / 2)
        lcdout(LCDREG_WINDOW_VERT_END, 159 - (160 - 96) / 2)
        ; Set entry mode (down-then-right)
        lcdout(LCDREG_ENTRYMODE, 0b0001000000110000)
    pop hl
    pop bc
    pop af
    cp a
    ret

;; resetLegacyLcdMode [Color]
;;  Sets the LCD to color mode. Call this before you call
;;  [[getLcdLock]].
resetLegacyLcdMode:
    push af
        ld a, (color_mode)
        cp 1
        jr nz, _
    pop af
    ret
_:  ld a, 1
    ld (color_mode), a
    push bc
    push hl
    ld a, i
    push af
    di
        call getCurrentThreadId
        call getThreadEntry
        ld a, 5
        add l, a
        ld l, a
        jr nc, _
        inc h
_:      set 3, (hl)
        ; Set BASEE = 0, both partial images = 1
        ld a, LCDREG_DISPCONTROL1
        out (PORT_LCD_CMD), a \ out (PORT_LCD_CMD), a
        ld c, PORT_LCD_DATA
        in a, (PORT_LCD_DATA) \ in l, (c)
        and 0b11001111 ; Partial images
        or  0b00000001 ; BASEE
        out (PORT_LCD_DATA), a
        out (c), l
        ; Set interlacing off
        lcdout(LCDREG_DRIVER_OUTPUTCONTROL1, 0b0000000000000000)
        ; Set window
        lcdout(LCDREG_WINDOW_HORIZ_START, 0)
        lcdout(LCDREG_WINDOW_HORIZ_END, 239)
        lcdout(LCDREG_WINDOW_VERT_START, 0)
        lcdout(LCDREG_WINDOW_VERT_END, 319)
        ; Set entry mode
        lcdout(LCDREG_ENTRYMODE, 0x10B8)
    pop af
    jp po, _
    ei
_:  pop hl
    pop bc
    pop af
    cp a
    ret

;; checkLegacyLcdMode [Color]
;;  Sets Z if the current thread is in legacy mode.
checkLegacyLcdMode:
    push hl
    push af
    ld a, i
    push af
    di
        call getCurrentThreadId
        call getThreadEntry
        ld a, 5
        add l, a
        ld l, a
        jr nc, _
        inc h
_:      bit 3, (hl)
        jr z, .legacyMode
.colorMode:
    pop af
    jp po, _
    ei
_:  pop af
    ld h, a
    or 1
    ld a, h
    pop hl
    ret
.legacyMode:
    pop af
    jp po, _
    ei
_:  pop af
    pop hl
    cp a
    ret

;; clipColorRectangle [Color]
;;  Draws a clipped rectangle of the specified size with the
;;  specified color in color mode.
;; Inputs:
;;  HL: X coordinate in pixels
;;  B: Y coordinate in pixels
;;  DE: width of the rectangle in pixels
;;  C: height of the rectangle in pixels
;;  IY: color of the rectangle in R5G6B5 format
;; Notes:
;;  The rectangle will be clipped to any LCD window already in
;;  place, and restore it afterwards.
clipColorRectangle:
    push af
        ld a, i
        di
        push af \ push hl \ push de \ push bc \ push ix
            dec de
            push de
                push bc
                    ld ix, -14
                    add ix, sp
                    push ix
                        ld a, LCDREG_WINDOW_HORIZ_START
                        ld b, 4
                        ld c, PORT_LCD_DATA
.registerSave:
                        out (PORT_LCD_CMD), a
                        out (PORT_LCD_CMD), a
                        in d, (c)
                        in e, (c)
                        ld (ix + 0), e
                        ld (ix + 1), d
                        inc ix
                        inc ix
                        inc a
                        djnz .registerSave
                    pop ix
                    ; Coordinates clipping
                pop bc
                ; Horizontal start
                push hl
                    call .clipX
                    ld a, LCDREG_WINDOW_VERT_START
                    ; I can't use the kernel's writeLcdRegister or readLcdRegister here because they destroy C
                    call .writeLcdReg
                    ; Horizontal end
                pop hl
            pop de
            add hl, de
            call .clipX
            ld a, LCDREG_WINDOW_VERT_END
            call .writeLcdReg
            ; Vertical start
            ld l, b
            ld h, 0
            ld a, 239
            cp b
            jr nc, +_
            ld a, b
            rla
            sbc a, a
            ld h, a
_:
            push hl
                call .clipY
                ld a, LCDREG_WINDOW_HORIZ_START
                call .writeLcdReg
                ; Vertical end
                ld e, c
                ld d, 0
            pop hl
            add hl, de
            call .clipY
            ld a, LCDREG_WINDOW_HORIZ_END
            call .writeLcdReg
            ; Actually draw the rect
            ld a, LCDREG_WINDOW_VERT_START
            call .readLcdReg
            ld c, l
            ld b, h
            ld a, LCDREG_CURSOR_COLUMN
            call .writeLcdReg
            ld    a, LCDREG_WINDOW_VERT_END
            call .readLcdReg
            or a
            sbc hl, bc
            inc hl
            ld e, l
            ld d, h
            ld a, LCDREG_WINDOW_HORIZ_START
            call .readLcdReg
            ld c, l
            ld b, h
            ld a, LCDREG_CURSOR_ROW
            call .writeLcdReg
            ld a, LCDREG_WINDOW_HORIZ_END
            call .readLcdReg
            or a
            sbc hl, bc
            ld a, l
            call mul16By8
            ld a, LCDREG_GRAM
            out (PORT_LCD_CMD), a
            out (PORT_LCD_CMD), a
            push iy \ pop de
            ld c, PORT_LCD_DATA
.drawLoop:
            out (c), d
            out (c), e
            dec hl
            ld a, h
            or l
            jr nz, .drawLoop
            ; Restore previous clipping window
            ld a, LCDREG_WINDOW_HORIZ_START
            ld b, 4
            ld c, PORT_LCD_DATA
.registerRcl:
            out (PORT_LCD_CMD), a
            out (PORT_LCD_CMD), a
            ld e, (ix + 0)
            ld d, (ix + 1)
            out (c), d
            out (c), e
            inc a
            inc ix
            inc ix
            djnz .registerRcl
        pop ix \ pop bc \ pop de \ pop hl \ pop af
        jp po, _
        ei
_:
    pop af
    ret

.clipX:
    ld e, (ix + 4)
    ld d, (ix + 5)
    call smax
    ld e, (ix + 6)
    ld d, (ix + 7)
    call smin
    ret
.clipY:
    ld e, (ix + 0)
    ld d, (ix + 1)
    call smax
    ld e, (ix + 2)
    ld d, (ix + 3)
    call smin
    ret

.readLcdReg:
    out (PORT_LCD_CMD), a
    out (PORT_LCD_CMD), a
    in a, (PORT_LCD_DATA)
    ld h, a
    in a, (PORT_LCD_DATA)
    ld l, a
    ret
.writeLcdReg:
    out (PORT_LCD_CMD), a
    out (PORT_LCD_CMD), a
    ld a, h
    out (PORT_LCD_DATA), a
    ld a, l
    out (PORT_LCD_DATA), a
    ret
#endif
