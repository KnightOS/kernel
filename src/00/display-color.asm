#ifndef COLOR

setLegacyLcdMode:
    ret
    
resetLegacyLcdMode:
clearColorLcd:
colorLcdOn:
colorLcdOff:
colorLcdWait:
readLcdRegister:
writeLcdRegister:
    or 1
    ld a, errUnsupported
    ret

#else
; Color screen is 320x240

;; writeLcdRegister [Color]
;;  Writes a 16-bit value to a color LCD register
;; Inputs:
;;  A: Register
;;  HL: Value
;; Comments:
;;  Destroys C
writeLcdRegister:
    out (0x10), a \ out (0x10), a
    ld c, 0x11
    out (c), h
    out (c), l
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
    out (0x10), a \ out (0x10), a
    ld c, 0x11
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
    ld a, 0x0D
    out (0x2A), a ; LCD delay
    lcdout(0x07, 0x0000) ; Reset Disp.Ctrl.1: LCD scanning, command processing OFF
    lcdout(0x06, 0x0000)
    ;lcdout(0x10, 0x07F1) ; Reset Pwr.Ctrl.1: Start RC oscillator, set voltages
    lcdout(0x11, 0x0007) ; Pwr.Ctrl.2: Configure voltages
    lcdout(0x12, 0x008C) ; Pwr.Ctrl.3: More voltages
    lcdout(0x13, 0x1800) ; Pwr.Ctrl.4: Take a wild guess
    lcdout(0x29, 0x0030) ; Pwr.Ctrl.7: I'm not an LCD engineer, don't ask me.
    call colorLcdWait
    lcdout(0x10, 0x0190) ; Init Pwr.Ctrl.1: Exit standby, fiddle with voltages, enable
    lcdout(0x11, 0x0227) ; Pwr.Ctrl.2: Configure voltages
    lcdout(0x06, 0x0001)
    call colorLcdWait
    call colorLcdWait
    lcdout(0x01, 0x0000) ; Reset Out.Ctrl.1: Ensure scan directions are not reversed
    lcdout(0x02, 0x0200) ; LCD Driving Control: Sets inversion mode=line inversion and disables it
    lcdout(0x03, 0x10b8) ; Init. Entry Mode: Cursor moves up/down, down, left, disable
    lcdout(0x08, 0x0202) ; Set front & back porches: 2 blank lines top & bottom
    lcdout(0x09, 0x0000) ; Reset Disp.Ctrl.3: Resets scanning stuff and off-screen voltage
    lcdout(0x0A, 0x0000) ; Disp.Ctrl.4: No FMARK
    lcdout(0x0C, 0x0000) ; RGB Disp.: Off
    lcdout(0x0D, 0x0000) ; FMARK position: Off
    lcdout(0x60, 0x2700) ; Driver Output Ctrl. 2
    lcdout(0x61, 0x0001) ; Base Image Display Ctrl: Use color inversion, no vertical scroll, reset voltage in non-display level
    lcdout(0x6A, 0x0000) ; Reset Vertical Scroll Ctrl.
    lcdout(0x90, 0x0010)
    lcdout(0x92, 0x0600)
    lcdout(0x95, 0x0200)
    lcdout(0x97, 0x0c00)
    lcdout(0x30, 0x0000) ; Gamma Control 1
    lcdout(0x31, 0x0305) ; Gamma Control 2
    lcdout(0x32, 0x0002) ; Gamma Control 3
    lcdout(0x35, 0x0301) ; Gamma Control 4
    lcdout(0x36, 0x0004) ; Gamma Control 5
    lcdout(0x37, 0x0507) ; Gamma Control 6
    lcdout(0x38, 0x0204) ; Gamma Control 7
    lcdout(0x39, 0x0707) ; Gamma Control 8
    lcdout(0x3C, 0x0103) ; Gamma Control 9
    lcdout(0x3D, 0x0004) ; Gamma Control 10
    lcdout(0x50, 0x0000) ; Horiz.Win.Start: 0
    lcdout(0x51, 0x00EF) ; Horiz.Win.End: 239 = 240-1
    lcdout(0x52, 0x0000) ; Vert.Win.Start: 0
    lcdout(0x53, 0x013F) ; Vert.Win.End: 319 = 320-1
    lcdout(0x2B, 0x000B) ; Set frame rate to 70
    lcdout(0x10, 0x1190) ; Init Pwr.Ctrl.1: Exit standby, fiddle with voltages, enable
    lcdout(0x07, 0x0001) ; Reset Disp.Ctrl.1: LCD scanning, command processing OFF
    call colorLcdWait
    call colorLcdWait
    lcdout(0x07, 0x0023) ; Reset Disp.Ctrl.1: LCD scanning, command processing OFF
    call colorLcdWait
    call colorLcdWait
    lcdout(0x07, 0x0133) ; Disp.Ctrl.1: LCD scan & light on, ready to enter standby
    ; Turn on backlight
    in a, (0x3A)
    set 5, a
    out (0x3A), a
    xor a
    ; TODO: Remember and restore backlight brightness
    lcdout(0x03, 0x10B8) ; Entry mode the way we want it
    ret

;; colorLcdOff [Color]
;;  Turns off the color LCD and backlight.
colorLcdOff:
    lcdout(0x07, 0x00)
    call colorLcdWait
    lcdout(0x10, 0x07F0)
    call colorLcdWait
    lcdout(0x10, 0x07F1)
    ; Turn off backlight
    in a, (0x3A)
    res 5, a
    out (0x3A), a
    ret

fastCopy: ; Draws a 96x64 monochrome buffer on a color screen
    call hasLcdLock
    ret nz
fastCopy_skipCheck:
    push hl \ push bc \ push de \ push af
        push iy \ pop hl
        ; Draws a 96x64 monochrome LCD buffer (legacy buffer) to the color LCD
        ld bc, 64 << 8 | 0x11 ; 64 rows in b, and the data port in c
        ld de, ((240 - 128) / 2) << 8 | 0 ; Top of the current window in d, 0xFF in e
.loop:
        ; Set cursor column to 0
        ld a, 0x21
        out (0x10), a \ out (0x10), a
        dec a ; ld a, 32
        out (c), a \ out (c), a
        ; Set window top and cursor row to d
        ;ld a, 0x20 ; (aka 32)
        out (0x10), a \ out (0x10), a
        out (c), e \ out (c), d ; Cursor row
        ld a, 0x50
        out (0x10), a \ out (0x10), a
        out (c), e \ out (c), d ; Window top
        ; Window bottom to d + 1
        inc d \ inc a
        out (0x10), a \ out (0x10), a
        out (c), e \ out (c), d
        ; Select data register
        ld a, 0x22
        out (0x10), a \ out (0x10), a
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
;;  Sets all pixels on the LCD to grey in color mode.
;; Inputs:
;;  IY: Color in 0bRRRRRGGGGGGBBBBB format
clearColorLcd:
    push af
    push hl
    push bc
    push de
        push iy \ pop de
        ; Set window
        ld a, 0x50
        ld hl, 0
        call writeLcdRegister
        inc a \ ld hl, 239
        call writeLcdRegister
        inc a \ ld hl, 0
        call writeLcdRegister
        inc a \ ld hl, 319
        call writeLcdRegister
        ; Set cursor
        ld a, 0x20
        ld hl, 0
        call writeLcdRegister
        inc a
        call writeLcdRegister
        inc a
        ; Select GRAM
        out (0x10), a \ out (0x10), a
        ld c, 0x11
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
    ret

;; setLegacyLcdMode [Color]
;;  Sets the LCD to legacy mode.
;; Notes:
;;  Legacy mode simulates a 96x64 monochrome screen with the help of [[fastCopy]]. Color
;;  graphics are not advised in legacy mode.
setLegacyLcdMode:
    call clearColorLcd
    push af
    push bc
    push hl
        ; Set up partial images
        ld a, 0x80
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
        ld a, 0x07
        out (0x10), a \ out (0x10), a
        ld c, 0x11
        in a, (0x11) \ in l, (c)
        or  0b00110000 ; Partial images
        and 0b11111110 ; BASEE
        out (0x11), a
        out (c), l
        ; Set interlacing on
        lcdout(0x01, 0b0000010000000000)
        ; Set window rows
        lcdout(0x52, (160 - 96) / 2)
        lcdout(0x53, 159 - (160 - 96) / 2)
        ; Set entry mode (down-then-right)
        lcdout(0x03, 0b0001000000110000)
    pop hl
    pop bc
    pop af
    ret

;; resetLegacyLcdMode [Color]
;;  Sets the LCD to color mode.
resetLegacyLcdMode:
    push af
    push bc
    push hl
        ; Set BASEE = 0, both partial images = 1
        ld a, 0x07
        out (0x10), a \ out (0x10), a
        ld c, 0x11
        in a, (0x11) \ in l, (c)
        and 0b11001111 ; Partial images
        or  0b00000001 ; BASEE
        out (0x11), a
        out (c), l
        ; Set interlacing off
        lcdout(0x01, 0b0000000000000000)
        ; Set window
        lcdout(0x50, 0)
        lcdout(0x51, 239)
        lcdout(0x52, 0)
        lcdout(0x52, 319)
        ; Set entry mode
        lcdout(0x03, 0x10B8)
    pop hl
    pop bc
    pop af
    ret
#endif
