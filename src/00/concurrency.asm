;; mutexLock [Concurrency]
;;  Atomically locks a mutex byte.  The application should
;;  initialize the byte with [[initMutex]].
;;  This routine blocks until the mutex is locked.
;;  Interrupts will be enabled to perform a context switch
;;  (if needed) and restored to their former state when done.
;; Inputs:
;;  HL: Pointer to mutex byte
mutexLock:
    push af
        ; There is not a set-and-check instruction on z80,
        ; so interrupts must be disabled for the check.
        ld a, i
        push af
_:          di
            ld a, (hl)
            inc a
            jr z, _
            ei
            call contextSwitch
            jr -_
_:          call getCurrentThreadID
            ld (hl), a
        pop af
        jp po, _
        ei
_:  pop af
    ret

;; mutexUnlock [Concurrency]
;;  Atomically unlocks a mutex byte.  If the mutex is not
;;  locked already by this thread, the thread will be killed!
;; Inputs:
;;  HL: Pointer to mutex byte
mutexUnlock:
    push af
        call getCurrentThreadID
        cp (hl)
        jr z, _
        jp killCurrentThread
_:  pop af
    ; jr initMutex

;; mutexInit [Concurrency]
;;  Initializes a byte at (HL) to be used with lockMutex
;;  and unlockMutex.
;; Inputs:
;;  HL: Pointer to mutex byte
mutexInit:
    ld (hl), 0xFF
    ret


;; condInit [Concurrency]
;;  Returns a pointer to a newly allocated condition variable.
;;  The application must [[free]] this variable when no threads
;;  are using it any longer.
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  IX: Pointer to condition variable (on success)
condInit:
    push af
    push bc
        ; Up to maxThreads-1 may wait.  (All threads waiting would never awaken.)
        ; The extra byte is the counter.
        ld bc, maxThreads
        call calloc
        jr nz, .fail
    pop bc
    pop af
    cp a
    ret
.fail:
    pop bc
    inc sp \ inc sp ;pop af
    ret

;; condWait [Concurrency]
;;  Blocks execution of the current thread until another thread
;;  notifies the condition variable.  A thread that intends to
;;  wait on a condition variable must first acquire a mutex and
;;  pass it to this routine.  The wait operation will atomically
;;  release the mutex and suspend the thread.  When the condition
;;  variable is notified, the thread is reawakened and the mutex
;;  is reacquired.  Interrupts will be enabled by this routine.
;; Inputs:
;;  HL: Pointer to locked mutex
;;  IX: Pointer to condition variable
condWait:
    push af
    push ix
        push de
            inc (ix)
            ld e, (ix)
            ld d, 0
            add ix, de
        pop de
        call getCurrentThreadID
        ld (ix), a
        call mutexUnlock
        call suspendCurrentThread
        call mutexLock
_:  pop ix
    pop af
    ret

;; condNotifyAll [Concurrency]
;;  Awakens all threads waiting on a condition variable.
;;  The threads should check to see if the condition that they were
;;  waiting for (perhaps if a queue has data) is still true!
;;  To avoid corner-case race conditions, the calling thread must
;;  lock the associated mutex before invoking this routine (and
;;  release it when done).
;; Inputs:
;;  IX: Pointer to condition variable
condNotifyAll:
    push af
    push bc
    push ix
        ld b, (ix)
_:      inc ix
        ld a, (ix)
        call resumeThread
        djnz -_
    pop ix
    ld (ix), 0
    pop bc
    pop af
    ret

;; condNotifyOne [Concurrency]
;;  Awakens the thread that has been waiting the longest on a
;;  condition variable.
;;  The threads should check to see if the condition that they were
;;  waiting for (perhaps if a queue has data) is still true!
;;  To avoid corner-case race conditions, the calling thread must
;;  lock the associated mutex before invoking this routine (and
;;  release it when done).
;; Inputs:
;;  IX: Pointer to condition variable
condNotifyOne:
    push af
    push bc
    push hl
        push ix \ pop hl
        xor a
        or (hl)
        jr z, .done
        dec (hl)
        ld b, (hl)
        inc hl
        ld a, (hl)
        call resumeThread
        xor a
        or b
        jr z, .done
        push de
            push hl \ pop de
            inc hl
            ldir
        pop de
.done:
    pop hl
    pop bc
    pop af
    ret
