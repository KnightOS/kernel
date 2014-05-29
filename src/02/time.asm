;; setClock [Time]
;;   Sets the internal clock.
;; Inputs:
;;   HL: Lower word of a 32-bit tick value
;;   DE: Upper word of a 32-bit tick value
;; Outputs:
;;   A: Preserved on success, error code on failure
;;   Z: Set on success, reset on failure
setClock:
#ifndef CLOCK
    ld a, errUnsupported
    or a
    ret
#else
    push af
        ld a, h
        out (PORT_CLOCKREG2_IN), a
        ld a, l
        out (PORT_CLOCKREG1_IN), a
        ld a, d
        out (PORT_CLOCKREG4_IN), a
        ld a, e
        out (PORT_CLOCKREG3_IN), a
        ld a, CLOCKCONTROL_ENABLE
        out (PORT_CLOCKCONTROL), a
        ld a, CLOCKCONTROL_ENABLE | CLOCKCONTROL_COMMAND
        out (PORT_CLOCKCONTROL), a
    pop af
    cp a
    ret
#endif
    
;; getClock [Time]
;;   Gets the internal clock.
;; Outputs:
;;   HL: Lower word of the 32-bit tick value
;;   DE: Upper word of the 32-bit tick value
;;   A: Preserved on success, error code on failure
;;   Z: Set on success, reset on failure
getTimeInTicks:
#ifndef CLOCK
    ld a, errUnsupported
    or a
    ret
#else
    push af
        in a, (PORT_CLOCKREG2_OUT)
        ld h, a
        in a, (PORT_CLOCKREG1_OUT)
        ld l, a
        in a, (PORT_CLOCKREG4_OUT)
        ld d, a
        in a, (PORT_CLOCKREG3_OUT)
        ld e, a
    pop af
    cp a
    ret
#endif

; The number of days before a given month
daysPerMonth:
    .dw 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ; Normal

daysPerMonthLeap:
    .dw 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 ; Leap year

;; convertTimeFromTicks [Time]
;;   Convert from ticks in seconds to time.
;;   The epoch is January 1st, 1997 (a Wednesday)
;; Inputs:
;;   HL: Lower word of tick value
;;   DE: Upper word of tick value
;; Outputs:
;;    D: Current second, from 0-59
;;    C: Current minute, from 0-59
;;    B: Current hour, from 0-23
;;    H: Current day, from 0-30
;;    L: Current month, from 0-11
;;   IX: Current year
;;    A: Day of the week, from 0-6 with 0 being sunday
;;    E: Garbage
convertTimeFromTicks:
    ld a, d
    ld c, e
    push hl \ pop ix

    ld de, 60
    call div32by16
    ;seconds on stack
    push hl                         
        ld de, 60
        call div32by16
        ; minutes on stack
        push hl                     
            ld de, 24
            ; hours on stack
            call div32by16
            push hl
                push ix \ pop hl
                inc hl \ inc hl \ inc hl
                ld c, 7
                call divHLbyC
                ; day of the week on stack
                push af             
                    push ix \ pop hl
                    push ix \ pop bc
                    call .getYearFromDays
                    call .getLeapsToDate
                    push bc \ pop hl
                    sbc hl, de
                    inc hl \ inc hl
                    push hl \ pop bc
                    call .getYearFromDays
                    ; Years on stack
                    push hl          
                        ex hl, de
                        push bc \ pop hl
                        call .getMonth
                        ld h, b
                        ld l, a
                    ; Years
                    pop ix          
                ; Day of the week
                pop de
                ld a, d
            ; Hours
            pop de
            ld b, e
        ; Minutes
        pop de
        ld c, e
    ; Seconds
    pop de
    ld d, e

    ret

; Inputs:
;   HL: The year
; Outputs: 
;   DE: The number of leap years (and thus days) since 1997
; 
; Does (a - 1)/4 - 3(a - 1)/400 - 484
.getLeapsToDate:
    push hl \ push af \ push bc 
        dec hl
        push hl 
            push hl \ pop de
            ld a, 3
            call mul16By8To24

            ld a, h
            ld c, l
            ld de, 400
            call divACByDE
            ld d, a
            ld e, c
        pop hl

        push de
            ld a, h
            ld c, l
            ld de, 4
            call divACByDE
            ld h, a
            ld l, c
        pop de

        sbc hl, de
        ld de, 484
        sbc hl, de
        ex hl, de
        
    pop bc \ pop af \ pop hl
    ret

; Inputs:
;   HL: The year
; Outsputs:
;    A: 1 if it is a leap year, 0 otherwise
; 
; Does getLeapsToDate( hl + 1 ) - getLeapsToDate( hl )
.isLeapYear:
    push hl \ push bc \ push de
        call .getLeapsToDate
        push de \ pop bc

        inc hl
        call .getLeapsToDate
        ex de, hl
        sbc hl, bc

        ld a, l
    pop de \ pop bc \ pop hl
    ret

; Inputs: HL, number of days
; Outputs: HL, the current year
;
; Does hl / 365
.getYearFromDays:
    push af \ push bc \ push de
        ld a, h
        ld c, l
        ld de, 365
        call divACByDE
        ld h, a
        ld l, c

        ld de, 1997
        add hl, de

    pop de \ pop bc \ pop af
    ret

; Inputs:
;   HL: the number of days
;   DE: the year
; Outputs:
;    A: The current month
;    B: The day of the month
.getMonth:
    push ix \ push hl \ push de
        push af \ push bc 
            ld a, h
            ld c, l
            ld de, 365
            call divACByDE
        pop bc \ pop af
        pop de \ push de

        ld ix, daysPerMonth

        ex hl, de
        call .isLeapYear
        cp 1
        jr nz, _  
        ld ix, daysPerMonthLeap
_:
        ld b, 11
        push bc
            ld bc, 22
            add ix, bc
        pop bc
_:
        ld h, (ix+1)
        ld l, (ix)
        ld a, b
        cp 0
        jr z, _
        call cpHLDE
        jr c, _
        dec ix \ dec ix
        dec b
        jr -_ 
_:
        ex hl, de
        sbc hl, de
        ld a, b
        ld b, l
    pop de \ pop hl \ pop ix

    ret

;; convertTimeToTicks [Time]
;;  Converts a time structure to seconds since epoch.
;; Inputs:
;;  D: Current second, from 0-59
;;  C: Current minute, from 0-59
;;  B: Current hour, from 0-23
;;  H: Current day, from 0-30
;;  L: Current month, from 0-11
;;  IX: Current year
;;  A: Day of the week, from 0-6 with 0 being sunday
;; Outputs:
;;   HL: Lower word of tick value
;;   DE: Upper word of tick value
convertTimeToTicks:
    ; TODO
    ret
    
;; getTime [Time]
;;   Gets the current time.
;; Outputs:
;;    D: Current second, from 0-59
;;    C: Current minute, from 0-59
;;    B: Current hour, from 0-23
;;    H: Current day, from 0-30
;;    L: Current month, from 0-11
;;   IX: Current year
;;    A: Day of the week, from 0-6 with 0 being sunday
;;    E: Garbage
getTime:
#ifndef CLOCK
    ld a, errUnsupported
    or a
    ret
#else
    call getTimeInTicks
    call convertTimeFromTicks
    cp a
    ret
#endif
