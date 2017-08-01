;; suspendDevice [System]
;;  Turns off the screen, enters low power mode, and halts system operation until the ON key is pressed.
suspendDevice:
    ld a, i
    push af
 #ifdef COLOR
    push hl
    push bc
    call checkLegacyLcdMode
    ld b, 0
    jr nz, _
    call resetLegacyLcdMode
    inc b
_:
    push bc
    call colorLcdOff
 #else
    ld a, LCD_CMD_SETDISPLAY
    out (PORT_LCD_CMD), a ; Disable LCD
 #endif
    di ; And interrupts, for now
    im 1 ; interrupt mode 1, for cleanliness
    in a, (PORT_INT_MASK)
    push af
        xor a
        out (PORT_INT_MASK), a ; Ack and disable all interrupts
        ld a, INT_ON
        out (PORT_INT_MASK), a ; Enable ON interrupt
        ei
        halt ; and halt
        di
    pop af
    out (PORT_INT_MASK), a
 #ifdef COLOR
    call colorLcdOn
    pop bc
    xor a
    cp b
    call nz, setLegacyLcdMode
    pop bc
    pop hl
 #else
    ld a, 1 + LCD_CMD_SETDISPLAY
    out (PORT_LCD_CMD), a ; Enable the screen
 #endif
    pop af
    ret po
    ei
    ret

unprotectRAM:
#ifdef CPU15
   xor a
   out (PORT_RAMEXEC_LOWLIMIT), a ; RAM Lower Limit ; out (25), 0
   dec a
   out (PORT_RAMEXEC_UPLIMIT), a ; RAM Upper Limit ; out (26), $FF
#else
   xor a
   out (PORT_RAM_PAGING), a
   out (PORT_FLASHEXCLUSION), a

   ld a, 0b000000001
   out (PORT_RAM_PAGING), a
   xor a
   out (PORT_FLASHEXCLUSION), a
#endif
   ret

unprotectFlash:
#ifdef CPU15
   ld a, 0xFF
   out (PORT_FLASHEXEC_LOWLIMIT), a ; Flash Lower Limit
   out (PORT_FLASHEXEC_UPLIMIT), a ; Flash Upper Limit
#else
   ld a, 0b000000010
   out (PORT_RAM_PAGING), a
   xor a
   out (PORT_FLASHEXCLUSION), a

   ld a, 0b000000111
   out (PORT_RAM_PAGING), a
   xor a
   out (PORT_FLASHEXCLUSION), a
#endif
   ret

; TODO: This could use some improvement
;; hexToHL [Miscellaneous]
;;  Converts a hexadecimal string to a number.
;; Inputs:
;;  HL: String pointer
;; Outputs:
;;  HL: Value
hexToHL:
    push de
    push af
        ; D
        ld a, (hl)
        or a
        jr z, .done
        call hexToA_doConvert
        rla \ rla \ rla \ rla
        ld d, a \ inc hl
        ld a, (hl)
        or a
        jr z, .done
        call hexToA_doConvert
        or d \ ld d, a \ inc hl
        ; E
        ld a, (hl)
        or a
        jr z, .done
        call hexToA_doConvert
        rla \ rla \ rla \ rla
        ld e, a \ inc hl
        ld a, (hl)
        or a
        jr z, .done
        call hexToA_doConvert
        or e
        ld e, a
        ex de, hl
.done:
    pop af
    pop de
    ret

;; hexToA [Miscellaneous]
;;  Converts a hexadecimal string to a number.
;; Inputs:
;;  HL: String pointer
;; Outputs:
;;  A: Value
hexToA:
    push bc
    push hl
        ld b, 0
_:      ld a, (hl)
        or a
        jr z, hexToA_ret

        rl b \ rl b \ rl b \ rl b
        call hexToA_doConvert
        or b
        ld b, a
        inc hl
        jr -_

hexToA_ret:
        ld a, b
    pop hl
    pop bc
    ret

hexToA_doConvert:
    cp 'a' ; Convert to lowercase
    jr c, _
        sub 'a' - 'A'
_:  cp 'A' ; Close gap between numbers and letter
    jr c, _
        sub 'A'-('9'+1)
_:  sub '0' ; To number
    ret

;; lcdDelay [Hardware]
;;  Blocks until the LCD is ready to receive data.
lcdDelay:
    push af
_:  in a, (PORT_LCD_CMD)
    rla
    jr c,-_
    pop af
    ret

;; getBatteryLevel [Hardware]
;;  Determines the approximate battery level.
;; Outputs:
;;  B: Battery level
;; Notes:
;;  This returns a value from 0-255. The precision of this value varies by calculator
;;  model. 6 MHz platforms will return a 0 or a 255. 15 MHz platforms will return
;;  0, 63, 127, 191, or 255 (multiples of 64), where 0 is critical.
getBatteryLevel:
#ifdef CPU15
    in a, (0x3A)
    or 0b10000000    ; Set bit 7 of port 3A (TI-OS does this)
    out (0x3A), a

    ld a, 0x06       ; Below threshhold 0x06 (critical)
    out (0x04), a
    nop \ nop \ nop \ nop \ nop \ nop
    in a, (2) \ rra
    ld b, 0
    jr nc, _

    ld a, 0x86       ; Below threshhold 0x86 (1 quarter)
    out (0x04), a
    nop \ nop \ nop \ nop \ nop \ nop
    in a, (2) \ rra
    ld b, 63
    jr nc, _

    ld a, 0x46       ; Below threshhold 0x46 (2 quarters)
    out (0x04), a
    nop \ nop \ nop \ nop \ nop \ nop
    in a, (2) \ rra
    ld b, 127
    jr nc, _

    ld a, 0xC6       ; Below threshold 0xC6 (3 quarters)
    out (0x04), a
    nop \ nop \ nop \ nop \ nop \ nop
    in a, (2) \ rra
    ld b, 191
    jr nc, _

    ld b, 255        ; Above all prior threshholds (4 quarters)

_:  ld a, 0x06
    out (0x04), a    ; Reset the battery threshold

    in a, (0x3A)
    and 0b01111111   ; Reset bit 7 of port 3A (TI-OS does this)
    out (0x3A), a

    in a, (0x3A)
    or 0b00010000    ; Set bit 4 of port 3A (TI-OS does this)
    out (0x3A), a
    nop \ nop \ nop \ nop \ nop \ nop
    and 0b11101111   ; Reset bit 4 of port 3A (TI-OS does this)
    out (0x3A), a

    ret
#else
    in a, (2) \ rra
    ld b, 0    ; Below critical threshold
    jr c, _
    ld b, 255  ; Above critical threshold
_:  ret
#endif

;; getBootPage [Miscellaneous]
;;  Gets the boot page for this calculator.
;; Outputs:
;;  A: Boot page
getBootPage:
   ld a, bootPage
   ret

;; getBootCodeVersionString [Miscellaneous]
;;  Gets the version string from the device's boot code.
;; Outputs:
;;  HL: String pointer
;; Notes:
;;  This allocates memory to hold the string. Deallocate it with [[free]] when you no longer need it.
getBootCodeVersionString:
    ld a, i
    push af
    di
        push af
        push bc
        push ix
        push de
            setBankA(bootPage)
            ld hl, 0x400F ; Location of boot code version string
            call strlen
            inc bc
            call malloc
            push ix \ pop de
            ldir
            push ix \ pop hl
        pop de
        pop ix
        pop bc
        pop af
    pop af
    ret po
    ei
    ret

#ifdef COLOR
color_pageBankA:
   push af
      bit BIT_BANKA_ISRAM_CPU15, a
      jr z, .zero
      ld a, 1
      out (PORT_MEMA_HIGH), a
   pop af \ push af
      res BIT_BANKA_ISRAM_CPU15, a
      out (PORT_BANKA), a
   pop af
   ret
.zero:
      xor a
      out (PORT_MEMA_HIGH), a
   pop af
   out (PORT_BANKA), a
   ret

color_pageBankB:
   push af
      bit BIT_BANKB_ISRAM_CPU15, a
      jr z, .zero
      ld a, 1
      out (PORT_MEMB_HIGH), a
   pop af \ push af
      res BIT_BANKB_ISRAM_CPU15, a
      out (PORT_BANKB), a
   pop af
   ret
.zero:
      xor a
      out (PORT_MEMB_HIGH), a
   pop af
   out (PORT_BANKB), a
   ret
#endif

;; indirect16HLDE [Miscellaneous]
;;  Performs HL = (HL) and DE = (DE).
;; Notes:
;;  This routine is useful as part of a callback for the callbackSort routine.
indirect16HLDE:
    ex hl, de
    call indirect16HL
    ex hl, de
    ; Fall through

;; indirect16HL [Miscellaneous]
;;  Performs HL = (HL)
indirect16HL:
    push af
        ld a, (hl)
        inc hl
        ld h, (hl)
        ld l, a
    pop af
    ret

;; cpHLDE_sort [Miscellaneous]
;;  Compares 16-bit integers at (HL) and (DE).  That is, calls indirect16HLDE,
;;  then calls cpHLDE.
;; Inputs:
;;  HL: Pointer to integer
;;  DE: Pointer to integer
;; Output:
;;  Flags: Same as z80 CP instruction.
;; Notes:
;;  This routine is useful as the callback for the [[callbackSort]] routine.
;;  It allows sorting a list of 16-bit numbers.
cpHLDE_sort:
    push hl
    push de
        call indirect16HLDE
        call cpHLDE
    pop de
    pop hl
    ret

;; sleep [Miscellaneous]
;;  Delays a number of milliseconds.
;; Inputs:
;;  HL: delay in milliseconds
;; Notes:
;;  117 or 121 T-states are added when called in 6 MHz mode to the delayed time,
;;  depending on the interrupt state upon calling.
sleep:
    push af                 ; +11
        ld a, i             ; +20
        di                  ; +24
        push af             ; +35
            push bc \ push hl ; +57
#ifdef CPU15
_:
                ; at 15 MHz, 1 ms = 15000 T-states
                in a, (PORT_CPUSPEED) ; +68
                or a        ; +72
                jr z, .pause_CPU6 ; +79/84
                
                ld b, 0     ; 7
                djnz $      ; 13 * 255 + 8 = 3323 -> 3331
                djnz $      ; 6654
                djnz $      ; 9977
                djnz $      ; 13300
                ld b, 129   ; 13304
                djnz $      ; 13 * 128 + 8 = 1672 -> 14976
                dec hl      ; 14982
                ld a, h     ; 14986
                or l        ; 14990
                jr nz, -_   ; 14997/15002
                
                jr ++_      ; +91/96
.pause_CPU6:
#endif
_:
                ; at 6 MHz, 1 ms = 6000 T-states
                ld b, 0     ; 7
                djnz $      ; 3331
                ld b, 203   ; 3338
                djnz $      ; 13 * 202 + 8 = 2634 -> 5972
                dec hl      ; 5978
                ld a, h     ; 5982
                or l        ; 5986
                jr nz, -_   ; 5993/5998
_:
            pop hl \ pop bc ; ++20
        pop af              ; ++30
        jp po, +_           ; ++40
        ei                  ; ++44
_:
    pop af                  ; ++50/54
    ret                     ; ++60/64
    
;; getKernelMajorVersion [Miscellaneous]
;;  Returns the kernel's major version number.
;; Outputs:
;;  HL: Major version number
;;  Z: Set on success, reset on error
;; Notes:
;;  Kernel versions are MAJOR.MINOR.PATCH. For kernel 1.6.2, this returns 1.
getKernelMajorVersion:
    push de \ push bc
        ld hl, kernel_version
        ld b, 10
        call strtoi
    pop bc \ pop de
    ret
    
;; getKernelMinorVersion [Miscellaneous]
;;  Returns the kernel's minor version number.
;; Outputs:
;;  HL: Minor version number
;;  Z: Set on success, reset on error
;; Notes:
;;  Kernel versions are MAJOR.MINOR.PATCH. For kernel 1.6.2, this returns 6.
getKernelMinorVersion:
    push de \ push bc
        ld hl, kernel_version
        ld b, '.'
        call strchr
        jr nz, .error
        inc hl
        ld b, 10
        call strtoi
.error:
    pop bc \ pop de
    ret
    
;; getKernelPatchNumber [Miscellaneous]
;;  Returns the kernel's patch number.
;; Outputs:
;;  HL: Patch number
;;  Z: Set on success, reset on error
;; Notes:
;;  Kernel versions are MAJOR.MINOR.PATCH. For kernel 1.6.2, this returns 2.
getKernelPatchNumber:
    push de \ push bc
        ld hl, kernel_version
        ld b, '.'
        call strchr
        jr nz, .error
        inc hl
        call strchr
        jr nz, .error
        inc hl
        ld b, 10
        call strtoi
.error:
    pop bc \ pop de
    ret
    
;; getKernelCommitsSinceTag [Miscellaneous]
;;  Returns how many commits have been made since the last kernel tag.
;; Outputs:
;;  HL: Number of commits
;;  Z: Set on success, reset on error
;; Notes:
;;  This will fail most of the time, as it is only included when running
;;  on a development kernel between releases.
getKernelCommitsSinceTag:
    push de \ push bc
        ld hl, kernel_version
        ld b, '.'
        call strchr
        jr nz, .error
        inc hl
        call strchr
        jr nz, .error
        inc hl
        ld b, '-'
        call strchr
        jr nz, .error
        inc hl
        ld b, 10
        call strtoi
.error:
    pop bc \ pop de
    ret
    
;; getKernelShortHash [Miscellaneous]
;;  Retreives the kernel's short hash string.
;; Outputs:
;;  HL: Pointer on short hash string
;;  Z: Set on success, reset on error
;; Notes:
;;  This allocates memory to store the string. Deallocate it with [[free]]
;;  when you're done.
;;  
;;  This will fail most of the time, as it is only included when running
;;  on a development kernel between releases.
getKernelShortHash:
    push de \ push bc
        ld hl, kernel_version
        ld b, '.'
        call strchr
        jr nz, .error
        inc hl
        call strchr
        jr nz, .error
        inc hl
        ld b, '-'
        call strchr
        jr nz, .error
        inc hl
        call strchr
        jr nz, .error
        inc hl
        ; calculate the hash's length
        call strlen
        ; skip the potential + that's at the end of the string
        push hl
            add hl, bc
            dec hl
            ld a, (hl)
            cp '+'
            jr nz, $+3
            dec bc
        pop hl
        call malloc
        jr nz, .error
        push ix \ pop de \ push de
            ldir
            ex de, hl
            dec hl
            ld (hl), 0
        pop hl
.error:
    pop bc \ pop de
    ret
    
;; isKernelDirty [Miscellaneous]
;;  Checks for a dirty kernel.
;; Outputs:
;;  Z: Set if dirty
;; Notes:
;;  A kernel is considered "dirty" if there were uncommitted changes when it was
;;  compiled.
isKernelDirty:
    push bc
        ld hl, kernel_version
        call strlen
        add hl, bc
        dec hl
        ld a, (hl)
        cp '+'
    pop bc
    ret
    
;; isAlphaNum [Strings]
;;  Tests if a character is a letter or a number.
;; Inputs:
;;  A: Character to test
;; Outputs:
;;  C: Set if the character is alphanumeric
isAlphaNum:
    cp '9' + 1
    jr nc, .notNum
    cp '0'
    ccf
    ret
.notNum:
    cp 'Z' + 1
    jr nc, .notUpperAlpha
    cp 'A'
    ccf
    ret
.notUpperAlpha:
    cp 'z' + 1
    jr nc, .notLowerAlpha
    cp 'a'
    ccf
    ret
.notLowerAlpha:
    or a
    ret
