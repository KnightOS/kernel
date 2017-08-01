;; mutexLock [Concurrency]
;;  Atomically locks a mutex.
;; Inputs:
;;  HL: Pointer to mutex byte
;; Notes:
;;  The application should have already initialized the mutex with [[initMutex]].
;;  
;;  This function blocks until the mutex is locked.
;;  
;;  Interrupts will be enabled to do a context switch (if needed) and will be
;;  restored to their original state afterwards.
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
;;  Atomically unlocks a mutex.
;; Inputs:
;;  HL: Pointer to mutex byte
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;; Notes:
;;  If the mutex is not locked already by this thread, an error will be returned.
mutexUnlock:
    push af
        call getCurrentThreadID
        cp (hl)
        jr z, _
        inc sp \ inc sp
    ld a, errMutexNotLocked
    or 1
    ret
_:  pop af
    ; jr mutexInit

;; mutexInit [Concurrency]
;;  Initializes a mutex byte at (HL).
;; Inputs:
;;  HL: Pointer to mutex byte
mutexInit:
    ld (hl), 0xFF
    ret


;; condInit [Concurrency]
;;  Returns a pointer to a new condition variable.
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  IX: Pointer to condition variable (on success)
;; Notes:
;;  This allocates memory for the condition variable. You should deallocate it
;;  with [[free]] when you're done.
condInit:
    push af
    push bc
        ; Up to max_threads-1 may wait.  (All threads waiting would never awaken.)
        ; The extra byte is the counter.
        ld bc, max_threads
        ld a, 1
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
;;  Blocks until another thread notifies the condition variable. 
;; Inputs:
;;  HL: Pointer to locked mutex
;;  IX: Pointer to condition variable
;; Notes:
;;  This function will enable interrupts.
;;  
;;  You must provide a pre-initialized mutex and condition variable to this
;;  function. This function will release the mutex and suspend the thread,
;;  then awaken it when the condition variable is notified. The mutex will
;;  then be reacquired.
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
;; Inputs:
;;  IX: Pointer to condition variable
;; Notes:
;;  Threads awoken by this mechanism should check to see if the condition variable
;;  they were waiting for is still true.
;;  
;;  The thread calling this function must lock the associated mutex before invoking
;;  this function, and release it afterwards.
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
;;  Awakens the thread that has been waiting the longest on a condition variable.
;; Inputs:
;;  IX: Pointer to condition variable
;; Notes:
;;  Threads awoken by this mechanism should check to see if the condition variable
;;  they were waiting for is still true.
;;  
;;  The thread calling this function must lock the associated mutex before invoking
;;  this function, and release it afterwards.
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
