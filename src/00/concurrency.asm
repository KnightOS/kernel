;; acquireMutex [Concurrency]
;;  Atomically locks a mutex byte.  The application should
;;  initialize the byte with [[initMutex]].
;;  This routine blocks until the mutex is acquired.
;;  Interrupts will be enabled to perform a context switch
;;  (if needed) and restored to their former state when done.
;; Inputs:
;;  HL: Pointer to mutex byte
acquireMutex:
    push af
        ; There is not a set-and-check instruction on z80,
        ; so interrupts must be disabled for the check.
        ld a, i
        push af
_:          di
            ld a, (hl)
            or a
            jr z, _
            ei
            call contextSwitch
            jr -_
_:          inc a
            ld (hl), a
        pop af
        jp po, _
        ei
_:  pop af
    ret

;; initMutex [Concurrency]
;;  Initializes a byte at (HL) to be used with acquireMutex
;;  and releaseMutex.
;; Inputs:
;;  HL: Pointer to mutex byte
initMutex:

;; releaseMutex [Concurrency]
;;  Atomically unlocks a mutex byte.  If the mutex is not
;;  locked already by this thread, the result is undefined!
;; Inputs:
;;  HL: Pointer to mutex byte
releaseMutex:
    ld (hl), 0
    ret
