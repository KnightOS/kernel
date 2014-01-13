#ifdef COLOR
; Color screen is 320x240

; Blinks the LCD backlight 10 times, then pauses
; For debugging
debug_blink:
    ld b, 10
.loop:
    push bc
        in a, (0x3A)
        set 5, a
        out (0x3A), a ; off
        call colorLcdWait
        in a, (0x3A)
        res 5, a
        out (0x3A), a ; on
        call colorLcdWait
    pop bc
    djnz .loop
    ret

; Destroys C
; A: Register
; HL: Value
writeLcdRegister:
    out (0x10), a \ out (0x10), a
    ld c, 0x11
    out (c), h
    out (c), l
    ret

; Destroys C
; A: Register
; HL: Value
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

colorLcdOn:
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
    lcdout(0x03, 0x10B8) ; Entry mode the way we want it
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

fastCopy: ; Draws a 96x64 monochrome buffer on a color screen
fastCopy_skipCheck:
    ; TODO: Thread locking
    ; TODO: Update original fastCopy docs
    push hl \ push bc \ push de \ push af
        ; Draws a 96x64 monochrome LCD buffer (legacy buffer) to the color LCD
        ld bc, 64 << 8 | 0x11 ; 64 rows in b, and the data port in c
        ld de, ((240 - 128) / 2) << 8 | 0xFF ; Top of the current window in d, 0xFF in e
        ld l, 0
.loop:
        ; Set cursor column to 0
        ld a, 0x21
        out (0x10), a \ out (0x10), a
        dec a ; ld a, 32
        out (c), a \ out (c), a
        ; Set window top and cursor row to d
        ;ld a, 0x20 ; (aka 32)
        out (0x10), a \ out (0x10), a
        out (c), l \ out (c), d ; Cursor row
        ld a, 0x50
        out (0x10), a \ out (0x10), a
        out (c), l \ out (c), d ; Window top
        ; Window bottom to d + 1
        inc d \ inc a
        out (0x10), a \ out (0x10), a
        out (c), l \ out (c), d
        ; Select data register
        ld a, 0x22
        out (0x10), a \ out (0x10), a
        ; Draw row
        push bc
            ld h, 12
.outerLoop:
            ld a, (iy)
            inc iy
            ld b, 8
            ; 8 pixels
.byteLoop:
            bit 7, a
            jr z, .white
.black:
            ; Output each pixel twice
            out (c), l \ out (c), l
            out (c), l \ out (c), l
            rla
            djnz .byteLoop
            jr .endLoop
.white:
            out (c), e \ out (c), e
            out (c), e \ out (c), e
            rla
            djnz .byteLoop
.endLoop:
            dec h
            jr nz, .outerLoop
        pop bc
        inc d
        djnz .loop
    pop af \ pop de \ pop bc \ pop hl
    ret

clearColorLcd:
    push af
    push hl
    push bc
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
        ld c, 240
.outerLoop:
        ld b, 160
.innerLoop:
        ; Two pixels per iteration
        ld a, 0b10100101
        out (0x11), a
        ld a, 0b00110100
        out (0x11), a
        ld a, 0b10100101
        out (0x11), a
        ld a, 0b00110100
        out (0x11), a
        djnz .innerLoop
        dec c
        jr nz, .outerLoop
    pop bc
    pop hl
    pop af
    ret

;; setLegacyLcdMode [Color]
;;  Sets up the LCD for use with the legacy [[fastCopy]] function on
;;  color models.
;; Notes:
;;  This function sets the display to use interlaced partial images, so
;; that each pixel written is two pixels wide (for the sake of speed).
;; It also prepares the windowing and entry modes for fastCopy.
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
