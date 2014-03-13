;; setClock [Time]
;;   Sets the internal clock.
;; Inputs:
;;   HL: Lower word of a 32-bit tick value
;;   DE: Upper word of a 32-bit tick value
;; Outputs:
;;   A: errUnsupported if there is no clock, 0 otherwise
setClock:
#ifndef CLOCK
    ld a, errUnsupported
    or a
    ret
#else
    push af
        ld a, h
        out (0x41), a
        ld a, l
        out (0x42), a
        ld a, d
        out (0x43), a
        ld a, e
        out (0x44), a
        ld a, 1
        out (0x40), a
        ld a, 3
        out (0x40), a
    pop af
    cp a
    ret
#endif
    
;; getClock [Time]
;;   Sets the internal clock.
;; Inputs:
;;   None
;; Outputs:
;;   HL: Lower word of the 32-bit tick value
;;   DE: Upper word of the 32-bit tick value
;;    A: errUnsupported if there is no clock, 0 otherwise
getTimeInTicks:
#ifndef CLOCK
    ld a, errUnsupported
    or a
    ret
#else
    push af
        in a, (0x45)
        ld h, a
        in a, (0x46)
        ld l, a
        in a, (0x47)
        ld d, a
        in a, (0x48)
        ld e, a
    pop af
    cp a
    ret
#endif

;; The number of days before a given month
#ifdef CLOCK
.daysPerMonth:
    .dw 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ; Normal
    .dw 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 ; Leap year
#endif

;; convertTimeFromTicks [Time]
;;   Convert from ticks to time
;;   Epoch is January 1st, 1997 (Wednesday)
;;   See https://github.com/torvalds/linux/blob/master/kernel/time/timeconv.c
;; Inputs:
;;   HL: Lower word of tick value
;;   DE: Upper word of tick value
;; Outputs:
;;    D: Current second, from 0-59
;;    C: Current minute, from 0-59
;;    B: Current hour, from 0-23
;;    H: Current day, from 1-31
;;    L: Current month, from 1-12
;;   IX: Current year
;;    A: errUnsupported if there is no clock, otherwise the 
;;       day of the week, from 0-6 with 0 being sunday
convertTimeFromTicks:
#ifndef CLOCK
    ld a, errUnsupported
    or a
    ret
#else
    ;; Time is in big-endian, have to convert to little-endian 
    ld a, e
    ld c, d
    ld b, l
    ld l, h
    ld h, b
    push hl \ pop ix

    ld de, 60
    call div32by16
    push hl                         ; seconds on stack
        ld de, 60
        call div32by16
        push hl                     ; minutes on stack
            ld de, 24
            call div32by16
            push hl                 ; hours on stack
                push ix \ pop hl
                call .getYearsFromDays
                ld de, 1997
                add hl, de
                push hl \ pop ix    ; Years
            pop hl
            ld b, l                 ; Hours
        pop hl
        ld c, l                     ; Minutes
    pop hl
    ld d, l                         ; Seconds

    ret

;; Inputs: HL, number of days
;; Outputs: HL, the number of years
;; 
;; This is inaccurate at the moment, doesn't consider leap years
.getYearsFromDays:
    push af \ push bc \ push de
        ld a, h
        ld c, l
        ld de, 365
        call divACByDE
        ld h, a
        ld l, c
    pop de \ pop bc \ pop af
    ret

#endif

; H: Day
; L: Month
; IX: Year
; B: Hour
; C: Minute
; D: Second
; A: Day of Week
; Output: HLDE: Ticks
convertTimeToTicks:
    ; TODO
    ret
    
; H: Day
; L: Month
; D: Year
; B: Hours
; C: Minutes
; E: Seconds
; A: Day of Week
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
