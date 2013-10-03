; Screen is 320x240
#ifdef COLOR

.equ lcdWidth 320
.equ lcdHeight 240

; Registers
.equ lrSync           0
.equ lrDriverCode         0

.equ lrOutCtrl1       1
.equ lcdReverseRow        0100h
.equ lcdInterlaced        0400h

.equ lrDriveCtrl      2
.equ lcdEOR           0100h
.equ lcdBDivC         0200h
.equ lcdDriveCtrlDefault  0200h

.equ lrEntryMode      3
.equ lcdAM            0008h   ; If set, the cursor moves left/right after every write.
.equ lcdCurMoveHoriz      lcdAM   ; Alias
.equ lcdRowInc        0010h   ; If unset, the cursor is instead decremented
.equ lcdColInc        0020h   
.equ lcdORG           0080h   ; If set, when window is changed, cursor is reset to corner
.equ lcdBGR           1000h
.equ lcdTRI           4000h   ; If set, 18-bit color is accepted
.equ lcdDFM           8000h   ; Unpacked mode: Write R, G, and B separately if TRI is set
.equ lcdDispCtrlDefault   1038h

.equ lrDispCtrl1      7
.equ lcdD0            0001h
.equ lcdPwr0          lcdD0
.equ lcdD1            0002h
.equ lcdPwr1          lcdD1
.equ lcdCl            0008h
.equ lcd8Colors       lcdCl
.equ lcdDte           0010h
.equ lcdGon           0020h
.equ lcdBaseE         0100h
.equ lcdPtde0         1000h
.equ lcdPartImg1Enable   lcdPtde0
.equ lcdPtde1         2000h
.equ lcdPartImg2Enable   lcdPtde1
.equ lcdDispCtrl1Default 0133h

.equ lrDispCtrl2      8
.equ lcdBpMask        000Fh
.equ lcdBpShift       0
.equ lcdFpMask        0F00h
.equ lcdFpShift       8
.equ lcdDispCtrl2Default  0202h

.equ lrDispCtrl3      9   ; Probably useless
.equ lcdDispCtrl3Default  0

.equ lrDispCtrl4      0Ah
.equ lcdDispCtrl4Default  0

.equ lrRgbCtrl1       0Ch ; Controls the RGB dumping mode, which we probably can't use.
.equ lcdRgbCtrl1Default   0

.equ lrFMarkPos       0Dh ; Controls the timing of the FMARK pulse, which we probably can't see.
.equ lcdFMarkPosDefault   0

.equ lrRgbCtrl2       0Ch ; Controls the RGB dumping mode, which we probably can't use.
.equ lcdRgbCtrl2Default   0

.equ lrPwrCtrl1      10h
.equ lcdStb          0001h   ; Sleep mode
.equ lcdSlp          0002h   ; Standby mode
.equ lcdApMask       0070h
.equ lcdApShift      4
.equ lcdApE          0080h   ; Enable power supply
.equ lcdBtMask       0300h   ; Voltage step-up adjust
.equ lcdBtShift      8
.equ lcdSap          1000h   ; Enable source driver
.equ lcdPwrCtrl1Default  1190h

.equ lrPwrCtrl2       11
.equ lcdPwrCtrl2Default   0227h

.equ lrPwrCtrl3       12h
.equ lcdPwrCtrl3Default   008Ch

.equ lrPwrCtrl4       13h
.equ lcdPwrCtrl4Default   1800h

.equ lrRow            20h
.equ lrCol            21h
.equ lrGram           22h

.equ lrPwrCtrl7       29h
.equ lcdPwrCtrl7Default   0030h

.equ lrFrameRate      2Bh
.equ lcdFrameRateDefault  000Bh

.equ lrGamma1         30h
.equ lcdGamma1Default     0000h
.equ lrGamma2         31h
.equ lcdGamma2Default     3050h
.equ lrGamma3         32h
.equ lcdGamma3Default     0002h
.equ lrGamma4         35h
.equ lcdGamma4Default     0301h
.equ lrGamma5         36h
.equ lcdGamma5Default     0004h
.equ lrGamma6         37h
.equ lcdGamma6Default     0507h
.equ lrGamma7         38h
.equ lcdGamma7Default     0204h
.equ lrGamma8         39h
.equ lcdGamma8Default     0707h
.equ lrGamma9         3Ch
.equ lcdGamma9Default    0103h
.equ lrGamma10        3Dh
.equ lcdGamma10Default    0004h

.equ lrWinTop        50h
.equ lrWinBottom      51h
.equ lrWinLeft        52h
.equ lrWinRight       53h

.equ lrGateScnCtrl          60h
.equ lcdScnMask         001Fh
.equ lcdScnShift            0
.equ lcdNlMask          03F0h
.equ lcdNlShift         8
.equ lcdLinesMask           lcdNlMask
.equ lcdLinesShift          lcdNlShift
.equ lcdGs              8000h
.equ lcdScanDir         lcdGs
.equ lcdGateScnCtrlDefault      2700h

.equ lrBaseImgDispCtrl      61h
.equ lcdRev             0001h   ; If set, reverse color mode
.equ lcdInverseMode         lcdRev
.equ lcdVle             0002h   ; If set, lrHorizScroll is enabled
.equ lcdHScrl           lcdVle
.equ lcdNdl             0004h

.equ lrHScrl                6Ah ; Horizontal scan control

.equ lcdPi1Pos          80h
.equ lcdPi1Start            81h
.equ lcdPi1End          82h
.equ lcdPi2Pos          83h
.equ lcdPi2Start            84h
.equ lcdPi2End          85h

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
    ld b, 10
.wait:
    push bc
        call colorLcdWait
    pop bc
    djnz .wait
    ret

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
    lcdout(lrDispCtrl1, 0)
    lcdout(lrPwrCtrl1, (7 << lcdBtShift) | lcdApE | (7 << lcdApShift) | lcdStb)
    lcdout(lrOutCtrl1, 0)
    lcdout(lrDriveCtrl, lcdDriveCtrlDefault)
    lcdout(lrEntryMode, lcdBGR | lcdRowInc | lcdColInc)
    lcdout(lrDispCtrl2, lcdDispCtrl2Default)
    lcdout(lrDispCtrl4, lcdDispCtrl4Default)
    lcdout(lrRgbCtrl1, lcdRgbCtrl1Default)
    lcdout(lrFMarkPos, lcdFMarkPosDefault)
    lcdout(lrGateScnCtrl, lcdGateScnCtrlDefault)
    lcdout(lrBaseImgDispCtrl, lcdInverseMode)
    lcdout(lrHScrl, 0)
    call colorLcdWait
    lcdout(lrPwrCtrl1, lcdPwrCtrl1Default)
    lcdout(lrPwrCtrl2, lcdPwrCtrl2Default)
    call colorLcdWait
    lcdout(lrPwrCtrl3, lcdPwrCtrl3Default)
    call colorLcdWait
    lcdout(lrPwrCtrl4, lcdPwrCtrl4Default)
    lcdout(lrPwrCtrl7, lcdPwrCtrl7Default)
    lcdout(lrFrameRate, lcdFrameRateDefault)
    call colorLcdWait
    lcdout(lrGamma1, lcdGamma1Default)
    lcdout(lrGamma2, lcdGamma2Default)
    lcdout(lrGamma3, lcdGamma3Default)
    lcdout(lrGamma4, lcdGamma4Default)
    lcdout(lrGamma5, lcdGamma5Default)
    lcdout(lrGamma6, lcdGamma6Default)
    lcdout(lrGamma7, lcdGamma7Default)
    lcdout(lrGamma8, lcdGamma8Default)
    lcdout(lrGamma9, lcdGamma9Default)
    lcdout(lrGamma10, lcdGamma10Default)
    lcdout(lrWinTop, 0)
    lcdout(lrWinBottom, lcdHeight - 1)
    lcdout(lrWinLeft, 0)
    lcdout(lrWinRight, lcdWidth - 1)
    lcdout(lrDispCtrl1, lcdDispCtrl1Default)
    ; Turn on backlight
    in a, (0x3A)
    set 5, a
    out (0x3A), a
    xor a
    ; Entry mode the way we want it
    lcdout(0x03, 0x10B8)
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

; 40 milliseconds-ish @ 6 MHz
colorLcdWait:
    ld b, 0x7F
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

fastCopy: ; Draws a 96x64 monochrome buffer on a color screen
fastCopy_skipCheck:
    ; TODO: Indent according to stack
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
    ld a, 0x20
    ld hl, 0
    call setLcdRegister
    inc a
    call setLcdRegister
    inc a
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
        call setLcdRegister
        inc a
        ; ld hl, 0
        call setLcdRegister
        inc a
        ld hl, 159
        call setLcdRegister
        inc a
        ld hl, 160
        call setLcdRegister
        inc a
        ld hl, 0
        call setLcdRegister
        inc a
        ld hl, 159 ; 95
        call setLcdRegister
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
#endif