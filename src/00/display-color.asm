#ifdef COLOR
; Destroys C
; A: Register
; HL: Value
setLcdRegister:
    out (0x10), a \ out (0x10), a
    ld c, 0x11
    out (c), h
    out (c), l
    ret

colorLcdOn:
    ; TODO: Research this more, it's probably not all required and we might want some of it done different.
    ; Could also probably be optimized if we didn't use this lcdout macro, but I'll save that for when the
    ; LCD is more well understood and everything is working.
    lcdout(0x01, 0x0000) ; Reset Out.Ctrl.1: Ensure scan directions are not reversed
    lcdout(0x02, 0x0200) ; LCD Driving Control: Sets inversion mode=line inversion and disables it
    lcdout(0x03, 0x1038) ; Init. Entry Mode: Cursor moves up/down, down, left, disable
    lcdout(0x08, 0x0202) ; Set front & back porches: 2 blank lines top & bottom
    lcdout(0x09, 0x0000) ; Reset Disp.Ctrl.3: Resets scanning stuff and off-screen voltage
    lcdout(0x0A, 0x0000) ; Disp.Ctrl.4: No FMARK
    lcdout(0x0C, 0x0000) ; RGB Disp.: Off
    lcdout(0x0D, 0x0000) ; FMARK position: Off
    lcdout(0x60, 0x2700) ; Driver Output Ctrl. 2
    lcdout(0x61, 0x0001) ; Base Image Display Ctrl: Use color inversion, no vertical scroll, reset voltage in non-display level
    lcdout(0x6A, 0x0000) ; Reset Vertical Scroll Ctrl.
    call colorLcdWait
    lcdout(0x10, 0x1190) ; Init Pwr.Ctrl.1: Exit standby, fiddle with voltages, enable
    lcdout(0x11, 0x0227) ; Pwr.Ctrl.2: Configure voltages
    call colorLcdWait
    lcdout(0x12, 0x008C) ; Pwr.Ctrl.3: More voltages
    call colorLcdWait
    lcdout(0x13, 0x1800) ; Pwr.Ctrl.4: Take a wild guess
    lcdout(0x30, 0x0030) ; Pwr.Ctrl.7: I'm not an LCD engineer, don't ask me.
    lcdout(0x2B, 0x000B) ; Set frame rate to 70
    call colorLcdWait
    ; Don't touch the gamma control ones, no one knows what they mean
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
    call colorLcdWait
    lcdout(0x50, 0x0000) ; Horiz.Win.Start: 0
    lcdout(0x51, 0x00EF) ; Horiz.Win.End: 239 = 240-1
    lcdout(0x52, 0x0000) ; Vert.Win.Start: 0
    lcdout(0x53, 0x013F) ; Vert.Win.End: 319 = 320-1
    call colorLcdWait
    lcdout(0x07, 0x0133) ; Disp.Ctrl.1: LCD scan & light on, ready to enter standby
    ; Turn on backlight
    in a, (0x3A)
    set 5, a
    out (0x3A), a
    ret

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

colorLcdWait:
    ; TODO: Wait about 40ms
    ret
#endif