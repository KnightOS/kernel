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

colorLcdOn:
    ld a, 0x07
    call readLcdRegister
    res 5, l
    res 4, l
    res 1, l
    res 0, l
    call readLcdRegister
    ld a, 0x11
    call readLcdRegister
    set 0, l
    set 1, l
    set 2, l
    call writeLcdRegister
    ld a, 0x12
    call readLcdRegister
    set 3, l
    set 2, l
    res 1, l
    res 0, l
    set 7, l
    call writeLcdRegister
    ld a, 0x29
    call readLcdRegister
    set 5, l
    set 4, l
    res 3, l
    res 2, l
    res 1, l
    res 0, l
    call writeLcdRegister
    ld a, 0x13
    call readLcdRegister
    set 4, h
    set 3, h
    res 2, h
    res 1, h
    res 0, h
    call writeLcdRegister
    ld a, 0x10
    call readLcdRegister
    res 2, h
    res 1, h
    res 0, h
    call writeLcdRegister
    call colorLcdWait
    ld a, 0x10
    call readLcdRegister
    set 7, l
    res 6, l
    res 5, l
    set 4, l
    res 2, h
    res 1, h
    set 0, h
    call writeLcdRegister
    ld a, 0x11
    call readLcdRegister
    res 6, l
    set 5, l
    res 4, l
    res 2, h
    set 1, h
    res 0, h
    call writeLcdRegister
    call colorLcdWait
    call colorLcdWait
    ;set regular display registers here (basee, entry...)
    ld a, 0x10
    call readLcdRegister
    set 4, h
    call writeLcdRegister
    ld a, 0x07
    call readLcdRegister
    res 1, l
    set 0, l
    call writeLcdRegister
    call colorLcdWait
    call colorLcdWait
    ld a, 0x07
    call readLcdRegister
    set 5, l
    set 1, l
    set 0, l
    call writeLcdRegister
    call colorLcdWait
    call colorLcdWait
    ld a, 0x07
    call readLcdRegister
    set 4, l
    call writeLcdRegister
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
    call writeLcdRegister
    inc a
    call writeLcdRegister
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
#endif
