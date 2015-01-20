;; clockSupported [Time]
;;   Returns whether the clock is supported.
;; Outputs:
;;    A: Preserved on success; error code on failure
;;    Z: Set when the clock is supported; reset otherwise
clockSupported:
#ifdef CLOCK
    cp a ; set Z
#else
    or 1 ; reset Z
    ld a, errUnsupported
#endif
    ret

;; setClock [Time]
;;   Sets the internal clock.
;; Inputs:
;;   HL: Lower word of a 32-bit tick value
;;   DE: Upper word of a 32-bit tick value
;; Outputs:
;;    A: Preserved on success, error code on failure
;;    Z: Set on success, reset on failure
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
;;    A: Preserved on success, error code on failure
;;    Z: Set on success, reset on failure
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

;; monthLength [Time]
;;   Computes the amount of days in a given month.
;; Inputs:
;;   HL: The year
;;    E: The month (0-11)
;; Outputs:
;;    A: The amount of days in this month
monthLength:
    ld a, e
    cp 1
    jr nz, +_ ; if not February, avoid the costly leap year computation
    call isLeapYear
_:  push hl \ push bc
        cp 1
        jr z, +_ ; if a = 1, so we have a leap year
        ld hl, .monthLengthNonLeap
        jr ++_
_:      ld hl, .monthLengthLeap
_:      ld b, 0
        ld c, e
        add hl, bc
        ld a, (hl)
    pop bc \ pop hl
    ret

; The number of days in a given month
.monthLengthNonLeap:
    .db 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
.monthLengthLeap:
    .db 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31

;; isLeapYear [Time]
;;   Determines whether the given year is a leap year.
;; Inputs:
;;   HL: The year
;; Outputs:
;;    A: 1 if it is a leap year; 0 if it is not
isLeapYear:
    push bc \ push de
        ; divisible by 400?
        ld a, h
        ld c, l
        ld de, 400
        call divACByDE ; remainder in hl
        ld a, h
        cp 0
        jr nz, .notDivisibleBy400
        ld a, l
        cp 0
        jr nz, .notDivisibleBy400
    pop de \ pop bc
    ld a, 1
    ret
.notDivisibleBy400:
        ; divisible by 100?
        ld c, 100
        push hl
            call divHLByC ; remainder in a
            cp 0
            jr nz, .notDivisibleBy100
        pop hl
    pop de \ pop bc
    ld a, 0
    ret
.notDivisibleBy100:
        pop hl
        ; divisible by 4?
        ld c, 4
        push hl
            call divHLByC ; remainder in a
            cp 0
            jr nz, .notDivisibleBy4
        pop hl
    pop de \ pop bc
    ld a, 1
    ret
.notDivisibleBy4:
        pop hl
    pop de \ pop bc
    ld a, 0
    ret

;; convertTimeFromTicks [Time]
;;   Convert from ticks in seconds to time.
;;   The epoch is January 1st, 1997 (a Wednesday).
;; Inputs:
;;   HL: Lower word of tick value
;;   DE: Upper word of tick value
;; Outputs:
;;    D: Current second (0-59)
;;    C: Current minute (0-59)
;;    B: Current hour, from 0-23
;;    H: Current day, from 0-30
;;    L: Current month, from 0-11
;;   IX: Current year
;;    A: Day of the week, from 0-6 with 0 being Sunday
;;    E: Garbage
;; Notes:
;;  This is unimplemented.
convertTimeFromTicks:
    ; TODO
    ret

;; convertTimeToTicks [Time]
;;   Converts a time structure to seconds since epoch.
;; Inputs:
;;    D: Current second (0-59)
;;    C: Current minute (0-59)
;;    B: Current hour (0-23)
;;    H: Current day (0-30)
;;    L: Current month (0-11)
;;   IX: Current year
;; Outputs:
;;   HL: Lower word of tick value
;;   DE: Upper word of tick value
;; Notes:
;;  This is unimplemented.
convertTimeToTicks:
    ; TODO
    ret

;; getTime [Time]
;;   Gets the current time.
;; Outputs:
;;    D: Current second (0-59)
;;    C: Current minute (0-59)
;;    B: Current hour (0-23)
;;    H: Current day (0-30)
;;    L: Current month (0-11)
;;   IX: Current year
;;    A: Day of the week, from 0-6 with 0 being Sunday
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
