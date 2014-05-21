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

lcdDelay:
    push af
_:    in a, (PORT_LCD_CMD)
    rla
    jr c,-_
    pop af
    ret

;; stringLength [Miscellaneous]
;;  Determines the length of a zero delimited string.
;; Inputs:
;;  HL: String pointer
;; Outputs:
;;  BC: String length
stringLength:
    push af
    push hl
        xor a
        ld b, a
        ld c, a
        cpir
        ; bc = -bc
        xor a \ sub c \ ld c, a \ sbc a, a \ sub b \ ld b, a
        dec bc
    pop hl
    pop af
    ret

;; getBatteryLevel [Miscellaneous]
;;  Determines the approximate battery level.
;; Outputs:
;;  B: Battery level
;; Notes:
;;  For 15MHz CPUs, B is a value from 0 to 4, where 0 is critical and 4 is full.
;;  For 6MHz CPUs, B is either 0 or 1, where 0 is critical and 1 is good.
getBatteryLevel:
#ifdef CPU15
#ifdef COLOR
   ; TODO
   ld b, 4
   ret
#else
    push af
        ld bc, 0x0000
        ; Reset battery threshold
        in a, (PORT_INT_TRIG)
        or 0b11 << BIT_MEM_TIMER_BATTERY
        out (PORT_MEM_TIMER), a
_:      push bc
            rrc c \ rrc c
            in a, (PORT_INT_TRIG)
            and 0b00111111
            or c
            out (PORT_MEM_TIMER), a
            in a, (PORT_CALC_STATUS)
            bit BIT_CALC_STATUS_BATTERY, a
            jr z, ++_
        pop bc
        inc c
        inc b
        ld a, b
        cp 4
        jr nz, -_

_:  pop af
    ret
_:  pop bc
    pop af
    ret
#endif
#else
    push af
        in a, (PORT_CALC_STATUS)
        and CALC_STATUS_BATTERY
        add a, a
        add a, a
        ld b, a
    pop af
    ret
#endif

;; compareStrings [Miscellaneous]
;;  Determines if two strings are equal, and checks alphabetical sort order.
;; Inputs:
;;  HL: String pointer
;;  DE: String pointer
;; Outputs:
;;  Z: Set if equal, reset if not equal
;;  C: Set if string HL is alphabetically earlier than string DE
compareStrings:
    ld a, (de)
    or a
    jr z, .end
    cp (hl)
    jr nz, .exit
    inc hl
    inc de
    jr compareStrings
.end:
    ld a, (hl)
    or a
.exit:
    ccf
    ret

;; stringCopy [Miscellaneous]
;;  Copies a string.
;; Inputs:
;;  HL: String pointer
;;  DE: Destination
stringCopy:
    push de
    push hl
    ex de, hl
_:  ld a, (de)
    ld (hl), a
    or a
    jr z, _
    inc hl \ inc de
    jr -_
_:  pop de
    pop hl
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
            call stringLength
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

;; randA [Miscellaneous]
;;  Returns a random byte in A.
;; Inputs:
;;  A': seed
;; Outputs:
;;  A: random byte
;;  A': reseeded
randA:
    push bc
        ex af, af'
        ld b, a
        ld a, r
        xor b
        rrca
        jr nc, +_
        xor 0b00111000
_:
        ld b, a
        ex af, af'
        ld a, b
    pop bc
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

;; compareStrings_sort [Miscellaneous]
;;  Compares strings at ((HL)) and ((DE)).  That is, calls indirect16HLDE,
;;  then calls compareStrings.
;; Inputs:
;;  HL: Pointer to string pointer
;;  DE: Pointer to string pointer
;; Outputs:
;;  Z: Set if equal, reset if not equal
;;  C: Set if string (HL) is alphabetically earlier than string (DE)
;; Notes:
;;  This routine is extremely useful as the callback for the [[callbackSort]] routine.
;;  It allows sorting a list of pointers to strings by the strings' sort order.
compareStrings_sort:
    push hl
    push de
        call indirect16HLDE
        call compareStrings
_:  pop de
    pop hl
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
;;  This routine is extremely useful as the callback for the [[callbackSort]] routine.
;;  It allows sorting a list of 16-bit numbers.
cpHLDE_sort:
    push hl
    push de
        call indirect16HLDE
        call cpHLDE
        jr -_

;; sleep [Miscellaneous ]
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
    
