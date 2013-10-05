; Screen is 320x240
#ifdef COLOR

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

;~AssemblyBandit~ http://briandm82.com
;10-4-13
;Based off the ILITEK9335 datasheet for a Power On Reset
;Written for KnightOS
ColorInit:
  jp POR
;--- DisplayWriteHL ---
;Writes hl to a
;inputs:
;a=address
;hl
DisplayWriteHL:
 ld c,$11
 ld b,a
 xor a
 out ($10),a
 ld a,b
 out ($10),a
 out (c),h
 out (c),l
 ret
 
;--- DisplayReadHL ---
;read hl at a
;inputs:
;a=address
;hl
DisplayReadHL:
 push af
 ld b,a
 xor a
 out ($10),a
 ld a,b
 out ($10),a
 in a,($11)
 ld h,a
 in a,($11)
 ld l,a
 pop af
 ret  
 
LCDWaiting:
colorLcdWait:
 ld b, 0xaF
 ld c, 0xFF
 ld hl, 0x8000
.Wloop:
 ld a, (hl)
 ld (hl), a
 dec bc
 ld a, c
 or b
 jp nz,.Wloop
 ret
 
POR: ;will start the lcd screen after a Power On Reset
 ld a,$07
 call DisplayReadHL
 res 5,l
 res 4,l
 res 1,l
 res 0,l
 call DisplayWriteHL
 ld a,$11
 call DisplayReadHL
 set 0,l
 set 1,l
 set 2,l
 call DisplayWriteHL
 ld a,$12
 call DisplayReadHL
 set 3,l
 set 2,l
 res 1,l
 res 0,l
 set 7,l
 call DisplayWriteHL
 ld a,$29
 call DisplayReadHL
 set 5,l
 set 4,l
 res 3,l
 res 2,l
 res 1,l
 res 0,l
 call DisplayWriteHL
 ld a,$13
 call DisplayReadHL
 set 4,h
 set 3,h
 res 2,h
 res 1,h
 res 0,h
 call DisplayWriteHL
 ld a,$10
 call DisplayReadHL
 res 2,h
 res 1,h
 res 0,h
 call DisplayWriteHL
 call LCDWaiting ;60ms
 ld a,$10
 call DisplayReadHL
 set 7,l
 res 6,l
 res 5,l
 set 4,l
 res 2,h
 res 1,h
 set 0,h
 call DisplayWriteHL
 ld a,$11
 call DisplayReadHL
 res 6,l
 set 5,l
 res 4,l
 res 2,h
 set 1,h
 res 0,h
 call DisplayWriteHL
 call LCDWaiting ;60ms
 call LCDWaiting ;60ms
;set regular display registers here (basee, entry...)
 ld a,$10
 call DisplayReadHL
 set 4,h
 call DisplayWriteHL
 ld a,$07
 call DisplayReadHL
 res 1,l
 set 0,l
 call DisplayWriteHL
 call LCDWaiting ;60ms
 call LCDWaiting ;60ms
 ld a,$07
 call DisplayReadHL
 set 5,l
 set 1,l
 set 0,l
 call DisplayWriteHL
 call LCDWaiting ;60ms
 call LCDWaiting ;60ms
 ld a,$07
 call DisplayReadHL
 set 4,l
 call DisplayWriteHL
;por finished, display on
 ret

colorLcdOn:
    ; TODO: Research this more, it's probably not all required and we might want some of it done different.
    ; Could also probably be optimized if we didn't use this lcdout macro, but I'll save that for when the
    ; LCD is more well understood and everything is working.
    lcdout(0x07, 0x0000) ; Reset Disp.Ctrl.1: LCD scanning, command processing OFF
    lcdout(0x06, 0x0000)
;    lcdout(0x10, 0x07F1) ; Reset Pwr.Ctrl.1: Start RC oscillator, set voltages
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

    ; Values found in TIOS, but not wikiti:
    ;lcdout(0x07, 0x0000) ; Settings modes, clears it for some reason?
    ;call colorLcdWait
    ;lcdout(0x10, 0x07F0) ; More power control
    ;call colorLcdWait
    ;lcdout(0x10, 0x07F1) ; Ditto
    ;call colorLcdWait
    ;lcdout(0x03, 0b1000000010111000) ; Entry mode the way we want it
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
;colorLcdWait:
    ld b, 0xaF
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
; TODO: This one might not work, the above is stolen from TIOS
    push hl
    push bc
        ld bc, 0x0080
_:      ; Waste 2560 cycles, 256 times
        ld hl, 0x1234
        djnz -_
        dec c
        jr nz, -_
    pop bc
    pop hl
    ret

fastCopy: ; Draws a 96x64 monochrome buffer on the screen
fastCopy_skipCheck:
    ld a, 0x20
    ld hl, 0
    call setLcdRegister
    inc a
    call setLcdRegister
    inc a
    out (0x10), a \ out (0x10), a
    
    push iy \ pop hl
    
    ld bc, 3 ; 0x300 (768) iterations
    ld de, 0xFF00
.outerLoop:
    ld a, (hl)
    push bc
        ld bc, 0x0811
.innerLoop: ; Draw 8 pixels
        bit 7, a
        jr z, .white
.black:
        out (c), e \ out (c), e
        rla
        djnz .innerLoop
        jr .endLoop
.white:
        out (c), d \ out (c), d
        rla
        djnz .innerLoop
.endLoop:
    pop bc
    inc hl
    djnz .outerLoop
    dec c
    jr nz, .outerLoop
    ret

clearLcd:
 ld b,10
RandLoop:
 ld a,r
 push bc
 call FillScreen
 ld b,10
ColorLoop:
 push bc
 call colorLcdWait
 pop bc
 djnz ColorLoop
 pop bc
 djnz RandLoop
 ld a, $ff
 call FillScreen
DisplayBlog: ;call to display blog info
;set window
 ld hl,95
 ld de,95+49
 ld a,$50
 call DisplayWriteHLDE ;set y
 ld hl,28
 ld de,28+263
 ld a,$52
 call DisplayWriteHLDE ;set x
 call SetCursor
 ld hl,ImageData+1
 ld ix,ImageData
 jp DrawImage
 
;call to draw 16 color rle image on screen
;window,cursor,gram,ImagePalette must be set
;hl points to data, ix points to signal
DrawImage:
 xor a
 out ($10),a
 ld a,$22
 out ($10),a
 ld a,(hl)
 inc hl
 cp (ix) ;if not signal, drawcolor
 jp z,Decompress ;if signal wait for other 2 bytes then loop draw
 push hl
 call DrawPixels
 pop hl
 jp DrawImage
Decompress: ;gets next two bytes to loop draw
 ld a,(hl)
 or a
 ret z
 inc hl
 ld b,a ; save loop count
 ld a,(hl)
 ld c,a
 inc hl
 push hl ;save hl for main loop
DCLoop:
 ld a,c
 push bc
 call DrawPixels ;draw color b times
 pop bc
 djnz DCLoop
 pop hl
 jp DrawImage
 
;--- DrawPixels ---
;Switches image depth and draws pixels
DrawPixels: ;a=data byte
 ld c,a
 ld b,4
B4Loop:
 xor a
 rl c
 rla
 rl c
 rla
 ld hl,ImagePalette
 ld d,0
 rla
 ld e,a
 add hl,de ;add color offset to palette
 ld a,(hl)
 out ($11),a
 inc hl
 ld a,(hl)
 out ($11),a ;draw color
 djnz B4Loop
 ret
 
FillScreen: ;fills screen with h=l screen color
 push af
 call SetFullWindow
 call SetCursor
;draw screen
 xor a
 out ($10),a
 ld a,$22
 out ($10),a
 ld de,19200
DBSLoop1:
 pop af
 push af
 out ($11),a
 out ($11),a
 out ($11),a
 out ($11),a
 out ($11),a
 out ($11),a
 out ($11),a
 out ($11),a
 dec de
 ld a,d
 or e
 jp nz,DBSLoop1
 pop af
 ret
DisplayWriteHLDE:
 ld c,$10
 ld b,$00
 out (c),b
 out (c),a
 inc c
 out (c),h
 out (c),l
 dec c
 inc a
 out (c),b
 out (c),a
 inc c
 out (c),d
 out (c),e
 ret 
SetCursor:
 ld hl,0
 ld de,0
 ld a,$20
 jp DisplayWriteHLDE
SetFullWindow:
 ;set full window
 ld hl,0
 ld de,239
 ld a,$50
 call DisplayWriteHLDE ;set y
 ld hl,0
 ld de,319
 ld a,$52
 jp DisplayWriteHLDE ;set x

ImagePalette:
.db $00, $00, $FF, $FF, $C5, $34, $7A, $69

;data size = 2796 bytes
;data begin
ImageData:
.db $04, $00, $00, $00, $05, $55, $50, $00, $05, $55, $00, $00, $00, $05, $04, $20
.db $55, $5A, $AA, $04, $13, $55, $00, $00, $00, $05, $55, $50, $00, $05, $55, $00
.db $00, $00, $05, $55, $55, $55, $5F, $E9, $04, $1A, $55, $5B, $00, $FF, $D5, $04
.db $12, $55, $05, $55, $55, $05, $50, $05, $50, $55, $55, $05, $55, $55, $05, $55
.db $55, $55, $70, $FD, $04, $1A, $55, $C0, $0F, $FF, $F9, $04, $12, $55, $05, $55
.db $55, $05, $50, $05, $50, $55, $55, $05, $55, $55, $05, $55, $55, $55, $8E, $57
.db $95, $04, $18, $55, $60, $FE, $A5, $55, $5F, $04, $12, $55, $05, $00, $05, $05
.db $55, $55, $04, $04, $05, $00, $05, $05, $55, $55, $50, $3A, $57, $D5, $55, $56
.db $C0, $35, $56, $C0, $35, $55, $6F, $E5, $54, $05, $55, $5A, $95, $5A, $00, $09
.db $55, $7F, $95, $55, $7F, $95, $73, $F9, $55, $55, $57, $95, $55, $FD, $55, $55
.db $A9, $57, $E5, $A9, $55, $55, $57, $F5, $55, $5A, $AC, $00, $55, $05, $00, $05
.db $05, $55, $55, $04, $04, $05, $00, $05, $05, $55, $55, $60, $E9, $57, $E5, $55
.db $B0, $00, $3D, $B0, $00, $3D, $AC, $03, $F9, $5C, $09, $55, $50, $36, $80, $00
.db $0F, $55, $43, $F5, $55, $43, $F5, $4F, $95, $55, $AA, $57, $E5, $55, $03, $95
.db $55, $02, $54, $3E, $FF, $95, $55, $54, $06, $AB, $C0, $0C, $00, $95, $05, $00
.db $05, $05, $05, $55, $00, $50, $05, $05, $00, $05, $05, $55, $55, $8A, $55, $57
.db $E5, $70, $FA, $A5, $5F, $FF, $E5, $5F, $FF, $FA, $BF, $43, $9F, $95, $83, $B0
.db $FF, $A5, $56, $D5, $8E, $BE, $55, $41, $7A, $3F, $55, $6A, $EE, $56, $E5, $6C
.db $FF, $F5, $56, $FF, $AC, $DB, $EB, $FE, $55, $5F, $FF, $FF, $BF, $FF, $95, $B9
.db $05, $00, $05, $05, $05, $55, $00, $50, $05, $05, $00, $05, $05, $55, $55, $CA
.db $55, $56, $E5, $CF, $A9, $55, $5B, $FD, $55, $5A, $FE, $55, $6F, $CF, $5B, $D5
.db $0F, $5F, $E9, $55, $55, $B5, $0D, $5F, $55, $41, $5C, $3E, $5B, $EF, $FE, $56
.db $E5, $83, $F5, $F5, $54, $F9, $A0, $D6, $FB, $FF, $D5, $5F, $DF, $FF, $A9, $55
.db $55, $BD, $05, $00, $05, $05, $00, $55, $50, $55, $55, $05, $00, $05, $05, $55
.db $56, $39, $55, $56, $F6, $3F, $A5, $55, $5A, $A5, $55, $5A, $95, $55, $6B, $0E
.db $56, $D7, $3D, $5F, $D5, $6B, $F5, $79, $3D, $6F, $55, $7E, $5C, $F9, $AB, $EF
.db $F9, $5B, $E5, $FF, $E5, $B5, $58, $F5, $B3, $D6, $F9, $6A, $E5, $53, $DF, $E5
.db $55, $55, $AA, $FD, $05, $00, $05, $05, $00, $55, $50, $55, $55, $05, $00, $05
.db $05, $55, $54, $35, $5A, $96, $F6, $35, $5A, $AF, $A5, $5A, $AF, $E9, $56, $AB
.db $FF, $FD, $56, $C0, $F5, $5A, $9A, $BE, $F5, $BD, $3D, $AF, $55, $6E, $5F, $D6
.db $BF, $D5, $55, $AB, $D7, $3D, $55, $B9, $63, $D5, $FF, $DB, $F6, $A9, $6A, $63
.db $5F, $FF, $FD, $6F, $FF, $FD, $05, $55, $55, $05, $55, $50, $00, $05, $55, $05
.db $55, $55, $05, $55, $54, $F5, $6A, $96, $B6, $E5, $5A, $FF, $E5, $5A, $FF, $FE
.db $5B, $FF, $FF, $F9, $56, $FF, $D5, $5B, $FF, $BF, $E6, $BE, $FD, $AE, $55, $63
.db $5F, $DA, $FF, $55, $55, $FF, $58, $F5, $55, $A9, $43, $96, $FF, $DB, $FA, $AA
.db $9A, $73, $5F, $FF, $FD, $7F, $FA, $A5, $05, $55, $55, $05, $55, $50, $00, $05
.db $55, $05, $55, $55, $05, $55, $54, $F5, $6B, $96, $BE, $E5, $5A, $F6, $E5, $5A
.db $F5, $7E, $5B, $BF, $94, $F5, $56, $FF, $55, $5B, $FF, $7F, $5A, $BF, $F5, $AD
.db $55, $53, $5F, $9E, $FF, $55, $55, $FE, $5C, $F5, $FD, $AD, $4F, $56, $F3, $DF
.db $EA, $BE, $96, $43, $5F, $FF, $FD, $7F, $55, $55, $00, $00, $00, $04, $06, $05
.db $00, $00, $00, $05, $55, $53, $E5, $BF, $E5, $AB, $FE, $A5, $6E, $BE, $A9, $6E
.db $4F, $56, $96, $D0, $E5, $55, $55, $69, $5F, $8F, $55, $5F, $E8, $E6, $F9, $55
.db $5A, $95, $7F, $8E, $56, $AA, $96, $93, $D6, $FD, $7F, $BF, $56, $FF, $6F, $EB
.db $F6, $36, $CD, $5F, $96, $FD, $FD, $55, $55, $00, $00, $00, $04, $06, $05, $00
.db $00, $00, $05, $55, $63, $D5, $BF, $E5, $57, $FF, $EA, $AF, $7F, $AA, $AF, $BF
.db $55, $56, $E3, $D6, $A5, $55, $BD, $5F, $8F, $55, $5F, $DC, $D7, $F5, $55, $57
.db $A5, $7F, $CD, $5F, $FF, $F6, $93, $D7, $AD, $7F, $FF, $57, $FF, $6F, $3B, $F8
.db $36, $BD, $6F, $97, $FE, $FD, $04, $07, $55, $00, $55, $00, $04, $06, $55, $73
.db $95, $69, $55, $5A, $EF, $FF, $FB, $EF, $FF, $FB, $3E, $55, $5A, $F3, $96, $A5
.db $56, $FD, $6F, $8F, $5A, $AA, $E0, $D7, $F5, $55, $54, $F5, $BF, $0D, $5F, $FF
.db $F6, $FF, $D7, $FD, $7E, $FD, $57, $EF, $6B, $FF, $F0, $F6, $BD, $7F, $54, $F7
.db $F9, $04, $07, $55, $00, $55, $00, $04, $06, $55, $CE, $5A, $95, $55, $6B, $D5
.db $B3, $FB, $D5, $B3, $FA, $FD, $FB, $FF, $CF, $5B, $EA, $BF, $FD, $6F, $3D, $BF
.db $FF, $6F, $57, $FF, $E5, $5F, $F6, $FD, $3D, $6F, $3F, $D6, $BF, $57, $D5, $5F
.db $F5, $57, $FD, $7F, $F7, $BF, $DB, $FD, $7E, $5C, $D7, $F5, $55, $55, $00, $55
.db $50, $00, $50, $50, $50, $00, $05, $55, $00, $55, $55, $56, $3D, $5B, $EA, $FD
.db $6F, $96, $0F, $FB, $D6, $0F, $FA, $F5, $F8, $FF, $CF, $5B, $EE, $BF, $3D, $6E
.db $3D, $FF, $FF, $6F, $5B, $FF, $FE, $63, $D7, $F9, $3D, $7F, $FF, $56, $FF, $7F
.db $55, $5F, $F5, $FF, $F5, $7F, $F7, $FF, $6F, $FD, $7E, $5C, $D7, $F5, $55, $55
.db $00, $55, $50, $00, $50, $50, $50, $00, $05, $55, $00, $55, $55, $56, $3D, $5B
.db $EA, $FD, $6F, $40, $FF, $DF, $C0, $FF, $DF, $B5, $FC, $FF, $FF, $6F, $EF, $FF
.db $0D, $7E, $3D, $FF, $FD, $6B, $5F, $FF, $FE, $73, $DF, $F5, $3D, $7F, $FD, $57
.db $ED, $7F, $BF, $5F, $D5, $FD, $55, $7F, $F7, $F5, $7F, $3D, $7D, $5F, $DF, $E5
.db $55, $55, $00, $55, $05, $55, $50, $55, $00, $00, $05, $50, $00, $00, $55, $56
.db $FD, $6F, $AB, $FD, $6F, $4F, $F5, $BF, $CF, $F5, $BF, $F5, $FE, $95, $7F, $BF
.db $55, $55, $0D, $BF, $F5, $FF, $56, $FF, $55, $55, $55, $8F, $5F, $D5, $35, $7D
.db $55, $7F, $F5, $FF, $FF, $5F, $F5, $FE, $57, $FF, $D5, $5F, $FF, $BD, $7D, $5F
.db $DF, $95, $55, $55, $00, $55, $05, $55, $50, $55, $00, $00, $05, $50, $00, $00
.db $55, $56, $F9, $6F, $56, $A5, $BF, $7D, $56, $FF, $BD, $56, $FF, $F5, $55, $55
.db $FF, $BE, $55, $55, $FD, $FF, $F5, $55, $5F, $F3, $95, $55, $56, $BD, $6F, $D5
.db $F9, $55, $56, $FF, $F5, $FE, $CF, $5F, $F6, $FF, $AF, $FC, $D5, $FF, $FE, $BD
.db $7D, $5B, $DF, $95, $55, $55, $50, $05, $00, $05, $55, $05, $55, $00, $55, $55
.db $00, $50, $05, $55, $FE, $FE, $55, $E6, $FE, $7D, $5B, $FE, $7D, $5B, $FE, $E9
.db $55, $6B, $FF, $FE, $55, $55, $AB, $FA, $F5, $55, $BF, $FF, $95, $7F, $FF, $FD
.db $7F, $95, $EA, $A5, $7F, $FF, $F7, $F9, $7D, $7F, $F7, $F7, $FF, $F8, $D7, $FF
.db $E5, $7E, $7D, $57, $EF, $D5, $55, $55, $50, $05, $00, $05, $55, $05, $55, $00
.db $55, $55, $00, $50, $05, $55, $5B, $FD, $55, $6F, $F9, $6F, $FF, $F5, $6F, $FF
.db $F5, $6B, $FF, $FF, $FB, $FD, $55, $55, $5B, $F5, $6F, $EB, $FF, $95, $FF, $FF
.db $FF, $EA, $FE, $55, $6B, $FF, $FF, $E9, $6F, $F5, $5B, $FF, $6F, $E5, $BF, $D5
.db $BF, $F9, $55, $5B, $FD, $55, $BF, $E5, $55, $55, $05, $00, $55, $50, $50, $55
.db $05, $05, $50, $05, $50, $55, $05, $55, $57, $F5, $55, $6F, $F5, $5F, $FF, $95
.db $5F, $FF, $95, $5B, $FF, $FE, $97, $FD, $55, $55, $56, $A5, $6F, $FF, $F9, $55
.db $FF, $FA, $9A, $57, $FD, $55, $5B, $FF, $FE, $55, $5F, $E5, $57, $FE, $6F, $95
.db $6A, $55, $BF, $A5, $55, $57, $F9, $55, $BF, $E5, $55, $55, $05, $00, $55, $50
.db $50, $55, $05, $05, $50, $05, $50, $55, $05, $04, $35, $55, $05, $50, $55, $00
.db $50, $50, $05, $05, $55, $05, $55, $55, $05, $04, $35, $55, $05, $50, $55, $00
.db $50, $50, $05, $05, $55, $05, $55, $55, $05, $04, $35, $55, $05, $00, $05, $55
.db $00, $55, $50, $50, $55, $50, $55, $50, $04, $36, $55, $05, $00, $05, $55, $00
.db $55, $50, $50, $55, $50, $55, $50, $04, $04, $55, $54, $00, $04, $05, $55, $00
.db $15, $04, $09, $55, $40, $05, $04, $06, $55, $5B, $03, $95, $55, $5B, $0E, $04
.db $12, $55, $05, $05, $55, $00, $50, $50, $00, $00, $05, $05, $00, $50, $05, $55
.db $55, $55, $54, $00, $04, $05, $55, $00, $15, $04, $09, $55, $40, $05, $04, $06
.db $55, $80, $00, $09, $55, $80, $00, $D5, $04, $11, $55, $05, $05, $55, $00, $50
.db $50, $00, $00, $05, $05, $00, $50, $05, $55, $55, $55, $54, $00, $04, $05, $55
.db $00, $15, $04, $09, $55, $40, $05, $04, $06, $55, $00, $00, $01, $55, $00, $00
.db $35, $04, $11, $55, $05, $04, $04, $50, $00, $55, $55, $50, $04, $04, $05, $55
.db $55, $55, $54, $00, $04, $10, $55, $40, $05, $04, $05, $55, $56, $00, $98, $02
.db $56, $00, $9C, $09, $04, $11, $55, $05, $04, $04, $50, $00, $55, $55, $50, $04
.db $04, $05, $55, $55, $55, $54, $00, $6C, $E5, $50, $01, $B5, $00, $15, $56, $C3
.db $A5, $54, $00, $6C, $E5, $56, $CE, $40, $05, $40, $06, $CE, $5B, $39, $57, $00
.db $54, $03, $57, $00, $5C, $0D, $55, $55, $58, $39, $55, $55, $B0, $E5, $55, $00
.db $1B, $39, $6C, $E5, $55, $55, $55, $05, $55, $00, $00, $05, $00, $55, $55, $00
.db $00, $05, $05, $04, $04, $55, $54, $00, $C0, $09, $50, $02, $05, $00, $15, $60
.db $00, $09, $54, $00, $80, $09, $58, $00, $C0, $05, $40, $08, $00, $A0, $02, $54
.db $00, $54, $00, $54, $00, $5C, $01, $55, $55, $80, $00, $95, $58, $00, $0E, $55
.db $00, $20, $02, $80, $09, $55, $55, $55, $05, $55, $00, $00, $05, $00, $55, $55
.db $00, $00, $05, $05, $04, $04, $55, $54, $00, $00, $0D, $50, $03, $05, $00, $15
.db $40, $00, $02, $54, $00, $00, $0D, $5C, $00, $00, $05, $40, $00, $00, $00, $03
.db $54, $00, $54, $00, $54, $00, $50, $01, $55, $56, $00, $00, $15, $50, $00, $00
.db $55, $04, $04, $00, $0D, $04, $07, $55, $05, $05, $00, $55, $05, $55, $05, $05
.db $04, $04, $55, $54, $00, $50, $01, $50, $00, $05, $00, $15, $80, $18, $03, $54
.db $00, $50, $0D, $50, $01, $40, $05, $40, $05, $00, $14, $03, $57, $00, $54, $03
.db $54, $00, $60, $0D, $55, $56, $00, $50, $25, $60, $05, $00, $95, $00, $14, $00
.db $50, $0D, $04, $07, $55, $05, $05, $00, $55, $05, $55, $05, $05, $04, $04, $55
.db $54, $00, $50, $01, $50, $00, $05, $00, $15, $C0, $14, $03, $54, $00, $50, $01
.db $50, $01, $40, $05, $40, $05, $00, $14, $00, $56, $00, $98, $02, $55, $55, $70
.db $09, $55, $57, $00, $50, $35, $70, $05, $00, $D5, $00, $14, $00, $50, $01, $55
.db $55, $55, $00, $00, $00, $05, $05, $50, $55, $55, $05, $05, $00, $55, $05, $55
.db $55, $55, $54, $00, $50, $01, $50, $03, $55, $00, $15, $00, $14, $00, $54, $00
.db $50, $01, $50, $01, $40, $05, $40, $05, $00, $14, $00, $55, $00, $00, $0D, $55
.db $55, $40, $09, $55, $54, $00, $50, $05, $40, $05, $00, $15, $00, $14, $00, $50
.db $01, $55, $55, $55, $00, $00, $00, $05, $05, $50, $55, $55, $05, $05, $00, $55
.db $05, $55, $55, $55, $54, $00, $50, $01, $50, $02, $55, $00, $15, $00, $14, $00
.db $54, $00, $50, $01, $50, $01, $40, $05, $40, $05, $00, $14, $00, $55, $80, $00
.db $05, $55, $55, $C0, $35, $55, $54, $00, $50, $05, $40, $05, $00, $15, $00, $14
.db $00, $50, $01, $55, $55, $55, $05, $55, $55, $05, $00, $55, $00, $50, $05, $55
.db $05, $50, $05, $55, $55, $55, $54, $00, $50, $01, $50, $01, $55, $00, $15, $00
.db $14, $00, $54, $00, $50, $01, $50, $01, $40, $05, $40, $05, $00, $14, $00, $55
.db $C0, $00, $0D, $55, $55, $00, $25, $55, $54, $00, $55, $55, $40, $05, $00, $15
.db $00, $14, $00, $50, $01, $55, $55, $55, $05, $55, $55, $05, $00, $55, $00, $50
.db $05, $55, $05, $50, $05, $55, $55, $55, $54, $00, $50, $01, $50, $01, $55, $00
.db $15, $55, $60, $00, $54, $00, $50, $01, $50, $01, $40, $05, $40, $05, $00, $14
.db $00, $56, $00, $98, $02, $55, $57, $00, $D5, $55, $54, $00, $55, $55, $40, $05
.db $00, $15, $00, $14, $00, $50, $01, $55, $55, $55, $05, $00, $05, $05, $50, $05
.db $05, $04, $05, $00, $05, $55, $55, $55, $54, $00, $50, $01, $50, $01, $55, $00
.db $15, $5B, $00, $00, $54, $00, $50, $01, $50, $01, $40, $05, $40, $05, $00, $14
.db $00, $57, $00, $54, $03, $55, $54, $00, $95, $55, $54, $00, $55, $55, $40, $05
.db $00, $15, $00, $14, $00, $50, $01, $55, $55, $55, $05, $00, $05, $05, $50, $05
.db $05, $04, $05, $00, $05, $55, $55, $55, $54, $00, $50, $01, $50, $01, $55, $00
.db $15, $70, $0C, $00, $54, $00, $50, $01, $50, $01, $40, $05, $40, $05, $00, $14
.db $00, $54, $00, $54, $00, $55, $5C, $03, $55, $55, $54, $00, $55, $55, $40, $05
.db $00, $15, $00, $14, $00, $50, $01, $55, $55, $55, $05, $00, $05, $05, $55, $05
.db $55, $05, $00, $05, $50, $50, $05, $55, $55, $55, $54, $00, $50, $01, $50, $01
.db $55, $00, $15, $C0, $24, $00, $54, $00, $50, $01, $50, $01, $40, $05, $40, $05
.db $00, $14, $00, $54, $00, $54, $00, $55, $60, $02, $55, $55, $54, $00, $50, $05
.db $40, $05, $00, $15, $00, $14, $00, $50, $01, $55, $55, $55, $05, $00, $05, $05
.db $55, $05, $55, $05, $00, $05, $50, $50, $05, $55, $55, $55, $54, $00, $50, $01
.db $50, $01, $55, $00, $15, $00, $14, $00, $54, $00, $50, $01, $50, $01, $40, $05
.db $40, $05, $00, $14, $00, $54, $00, $54, $00, $55, $70, $0D, $55, $55, $54, $00
.db $50, $05, $40, $05, $00, $15, $00, $14, $00, $50, $01, $55, $55, $55, $05, $00
.db $05, $05, $50, $00, $05, $55, $55, $50, $55, $05, $05, $55, $55, $55, $54, $00
.db $50, $01, $50, $01, $55, $00, $15, $00, $14, $00, $54, $00, $50, $01, $50, $01
.db $40, $05, $40, $05, $00, $14, $00, $54, $00, $54, $00, $55, $80, $05, $55, $55
.db $54, $00, $50, $05, $40, $05, $00, $15, $00, $14, $00, $50, $01, $55, $55, $55
.db $05, $00, $05, $05, $50, $00, $05, $55, $55, $50, $55, $05, $05, $55, $55, $55
.db $54, $00, $50, $01, $50, $01, $55, $00, $15, $00, $14, $00, $54, $00, $50, $01
.db $50, $01, $40, $05, $40, $05, $00, $14, $00, $57, $00, $54, $03, $55, $C0, $25
.db $55, $40, $17, $00, $50, $35, $70, $05, $00, $D5, $00, $14, $00, $50, $01, $55
.db $55, $55, $05, $55, $55, $05, $05, $50, $55, $05, $00, $50, $05, $55, $05, $55
.db $55, $55, $54, $00, $50, $01, $50, $01, $55, $00, $15, $00, $14, $00, $54, $00
.db $50, $01, $50, $01, $40, $05, $40, $05, $00, $14, $00, $56, $00, $98, $02, $56
.db $00, $D5, $55, $40, $16, $00, $50, $25, $60, $05, $00, $95, $00, $14, $00, $50
.db $01, $55, $55, $55, $05, $55, $55, $05, $05, $50, $55, $05, $00, $50, $05, $55
.db $05, $55, $55, $55, $54, $00, $00, $0D, $50, $01, $55, $00, $15, $C0, $00, $00
.db $54, $00, $50, $01, $5C, $00, $00, $05, $40, $05, $00, $14, $00, $55, $00, $00
.db $02, $57, $00, $00, $05, $40, $15, $00, $00, $15, $60, $00, $00, $55, $00, $14
.db $00, $50, $01, $55, $55, $55, $00, $00, $00, $05, $00, $05, $50, $05, $00, $55
.db $50, $55, $05, $55, $55, $55, $54, $00, $C0, $09, $50, $01, $55, $00, $15, $80
.db $0C, $00, $54, $00, $50, $01, $58, $00, $C0, $05, $40, $05, $00, $14, $00, $55
.db $80, $00, $09, $57, $00, $00, $05, $40, $15, $80, $00, $95, $58, $00, $02, $55
.db $00, $14, $00, $50, $01, $55, $55, $55, $00, $00, $00, $05, $00, $05, $50, $05
.db $00, $55, $50, $55, $05, $55, $55, $55, $54, $00, $6C, $E5, $50, $01, $55, $00
.db $15, $6C, $E4, $00, $54, $00, $50, $01, $56, $CE, $40, $05, $40, $05, $00, $14
.db $00, $55, $5B, $03, $95, $54, $00, $00, $05, $40, $15, $6C, $09, $55, $56, $B0
.db $E5, $55, $00, $14, $00, $50, $01, $55, $55, $55, $04, $00




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
    ld a, 0b00000100
    out (0x11), a
    ld a, 0b01111111
    out (0x11), a
    ld a, 0b00000100
    out (0x11), a
    ld a, 0b01111111
    out (0x11), a
    djnz .innerLoop
    dec c
    jr nz, .outerLoop
    ret

; Emulate a 96x68 monochrome screen, assuming you use the fastCopy provided above
setLcdCompatibleMode:
    ld hl, 0
    ld a, 0x50
    call setLcdRegister
    ld hl, 63
    inc a
    call setLcdRegister
    ld hl, 0
    inc a
    call setLcdRegister
    ld hl, 95
    inc a
    call setLcdRegister
    ret
#endif
