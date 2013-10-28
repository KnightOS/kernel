; Returns the ID of the thread that will launch next
getNextThreadID:
    push hl ; Don't care about the data getThreadEntry provides
        push bc
            ld a, (lastThreadId)
_:          inc a
            and threadRangeMask
            ld b, a ; Don't want the error getThreadEntry provides
            call getThreadEntry
            ld a, b
            jr z, -_
        pop bc
    pop hl
    ret

;; getCurrentThreadID [Threading]
;;  Gets the ID of the currently executing thread.
;; Outputs:
;;  A: Thread ID
getCurrentThreadID:
    push hl
        ld a, (currentThreadIndex)
        cp nullThread
        jr z, +_
        cp 0xFE
        jr z, ++_
        add a, a
        add a, a
        add a, a
        ld h, 0x80
        ld l, a
        ld a, (hl)
    pop hl
    ret
_:  pop hl
    jp getNextThreadID ; call \ ret
_:  pop hl
    ld a, 0xFE ; TODO: Dynamic library deallocation
    ret

;; startThread [Threading]
;;  Starts a new thread.
;; Inputs:
;;  HL: Pointer to thread executable
;;  B: Desired stack size / 2
;;  A: Thread flags
;; Outputs:
;;  A: ID of new thread (on success); Error code (on failure)
;;  Z: Set if successful, reset otherwise
;; Notes:
;;  If you wish to manipulate this thread before it executes (to set the initial value
;;  of the registers, for instance), disable interrupts before calling startThread, and
;;  re-enable them when you're ready to start the thread. If you want to postpone
;;  starting the thread for an extended period of time, call [[suspendThread]] before
;;  re-enabling interrupts, and [[resumeThread]] when you're ready to start it.
startThread:
    push af
        ld a, (activeThreads)
        cp maxThreads
        jr c, _
        jr z, _
        ld a, errTooManyThreads
    inc sp \ inc sp
    ret
_:      di
        ex de, hl
        ld a, (currentThreadIndex)
        push af
            ld a, (activeThreads)
            ld (currentThreadIndex), a ; Set the current thread to the new one so that allocated memory is owned appropraitely
            add a, a \ add a, a \ add a, a
            ld hl, threadTable
            add a, l
            ld l, a
            call getNextThreadID
            ld (lastThreadId), a
            ; A is now a valid thread id, and hl points to the next-to-last entry
            ; DE is address of code, B is stack size / 2
            ld (hl), a \ inc hl ; *hl++ = a
            ld (hl), e \ inc hl \ ld (hl), d \ inc hl
            ; Allocate a stack
            push hl
                push ix
                    ld a, b
                    add a, b
                    ld b, 0
                    add a, 24 ; Required minimum stack size for system use
                    ld c, a
                    jr nc, $+3 \ inc b
                    call malloc
                    jr nz, startThread_mem
                    push ix \ pop hl
                    dec ix \ dec ix
                    ld c, (ix) \ ld b, (ix + 1)
                    dec bc
                    add hl, bc
                    push de
                        ld de, killCurrentThread
                        ld (hl), d \ dec hl \ ld (hl), e ; Put return point on stack
                    pop de
                    dec hl \ ld (hl), d \ dec hl \ ld (hl), e ; Put entry point on stack
                    ld bc, 20 ; Size of registers on the stack
                    or a \ sbc hl, bc
                    ld b, h \ ld c, l
                pop ix
            pop hl
        pop af
        ld (currentThreadIndex), a
        ld (hl), c \ inc hl \ ld (hl), b \ inc hl ; Stack address
    pop af \ ld (hl), a \ inc hl ; Flags
    ld a, l
    sub 6
    ld l, a
    ld a, (activeThreads)
    inc a \ ld (activeThreads), a
    ld a, (hl)
    cp a
    ret

startThread_mem: ; Out of memory
                pop af
            pop af
        pop af
        ld (currentThreadIndex), a
    pop af
    ld a, errOutOfMem
    or 1
    ret

;; killCurrentThread [Threading]
;;  Kills the currently executing thread.
;; Notes:
;;  In most cases, it is preferrable to call [[exitThread]], which will use
;;  the exit function specified by the caller.
;;
;;  This function cleans up all resources owned by that thread, including
;;  allocated memory, loaded libraries, file handles, etc. This function
;;  will never return; invoke it with `jp killCurrentThread`.
killCurrentThread:
    di
    ; The stack is going to be deallocated, so let's move it
    ld sp, userMemory ; end of kernelGarbage
    ld a, (currentThreadIndex)
    add a, a
    add a, a
    add a, a
    ld hl, threadTable
    add a, l
    ld l, a
    ld a, (hl)
    ; HL points to old thread in table
    push af
        push hl
            ld a, (currentThreadIndex)
            inc a
            add a, a
            add a, a
            add a, a
            ld hl, threadTable
            add a, l
            ld l, a
            push hl
                push hl \ pop bc
                ld hl, threadTable + threadTableSize
                or a
                sbc hl, bc
                push hl \ pop bc
            pop hl
        pop de
        ldir

    pop af
    ; A = Old thread ID
    ; Clear old semaphores/signals.
    push hl \ push bc ; In case there are old signals, we don't want them!
_:      call readSignalAsThread
        jr z, -_
    pop bc \ pop hl
    ; Deallocate all memory belonging to the thread
killCurrentThread_Deallocate:
    ld ix, userMemory
killCurrentThread_DeallocationLoop:
    cp (ix)
    inc ix
    ld c, (ix)
    inc ix
    ld b, (ix)
    inc ix
    jr nz, _
    call free
    jr killCurrentThread_Deallocate
_:  inc ix \ inc ix
    inc bc \ inc bc
    add ix, bc
    dec ix \ dec ix
    jr c, killCurrentThread_DeallocationDone
    jr killCurrentThread_DeallocationLoop

killCurrentThread_DeallocationDone:

    ld hl, activeThreads
    dec (hl)
    xor a
    ld (currentThreadIndex), a
    jp contextSwitch_search

;; killThread [Threading]
;;  Kills the specified thread.
;; Inputs:
;;  A: Thread ID
;; Outputs:
;;  A: Error code (on failure)
;;  Z: Set if successful, reset otherwise
;; Notes:
;;  This function cleans up all resources owned by that thread, including
;;  allocated memory, loaded libraries, file handles, etc.
killThread:
    push bc
    ld c, a
    push af
    ld a, i
    push af
    push hl
    push de
    push ix
    di
    ld hl, threadTable
    ld a, (activeThreads)
    ld b, a
    ld d, 0
killThread_SearchLoop:
    ld a, (hl)
    cp c
    jr z,++_
    ld a, 8
    add a, l
    ld l, a
    inc d
    djnz killThread_SearchLoop
    ; Thread ID not found
    pop ix
    pop de
    pop hl
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    or a
    ld a, errNoSuchThread
    ret

_:  ; HL points to old thread in table
    push af
    push hl
        ld a, d
        inc a
        add a, a
        add a, a
        add a, a
        ld hl, threadTable
        add a, l
        ld l, a
        push hl
            push hl \ pop bc
            ld hl, threadTable + threadTableSize
            or a
            sbc hl, bc
            push hl \ pop bc
        pop hl
    pop de
    ldir
    pop af
    ; A = Old thread ID
    ; Clear old semaphores/signals.
    push hl \ push bc
_:      call readSignalAsThread
        jr z, -_
    pop bc \ pop hl
    ; Deallocate all memory belonging to the thread
    ld ix, userMemory
killThread_DeallocationLoop:
    cp (ix)
    inc ix
    ld c, (ix)
    inc ix
    ld b, (ix)
    inc ix
    jr nz, _
    call free
_:  inc ix \ inc ix
    inc bc \ inc bc
    add ix, bc
    dec ix \ dec ix
    jr c, killThread_DeallocationDone
    jr killThread_DeallocationLoop

killThread_DeallocationDone:
    ld hl, activeThreads
    dec (hl)
    ld b, (hl)
    ld a, (currentThreadIndex)
    dec b
    cp a
    jr nz, _
    dec a
    ld (currentThreadIndex), a
_:  pop ix
    pop de
    pop hl
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    cp a
    ret

;; launchProgram [Threading]
;;  Loads the specified file into memory as a program and starts a
;;  new thread for it.
;; Inputs:
;;  DE: Path to executable file
;; Outputs:
;;  A: Thread ID (on success), error code (on failure)
;;  Z: Set if successful, reset otherwise
launchProgram:
    push bc
    ld a, i
    push af
    di
    push hl
    push de
    push ix
        call openFileRead
        jr nz, .error

        push de
            call getStreamInfo
            dec bc
            dec bc
            ld a, (currentThreadIndex)
            push af
                ld a, nullThread
                ld (currentThreadIndex), a ; The null thread will allocate memory to the next thread
                call malloc
                jr nz, .error_pop2
            pop af
            ld (currentThreadIndex), a
        pop de

        push ix
            call streamReadByte ; Thread flags
            push af
                call streamReadByte ; Stack size
                ld c, a
                push bc
                    call streamReadToEnd ; Read entire file into memory
                    call closeStream
                pop bc
                ld b, c
            pop af
        pop hl
        call startThread
        jr nz, .error
    ld b, a
    pop ix
    pop de
    pop hl
    pop af
    jp po, _
    ei
_:  ld a, b
    pop bc
    cp a
    ret
.error_pop2:
    inc sp \ inc sp
.error_pop1:
    inc sp \ inc sp
.error:
    pop ix
    pop de
    pop hl
    ld b, a
    pop af
    jp po, _
    ei
_:  or 1
    ld a, b
    pop bc
    ret

;; exitThread [Threading]
;;  Immediately terminates the running thread. This function will never return;
;;  call it with `jp exitThread`.
;; Notes:
;;  This is preferred to [[killThread]], since it will go through the caller-set
;;  exit function. This is often [[killThread]] anyway, but it may be set to a
;;  custom value by the code that intitialized the thread.
exitThread:
    ; Not returning; we can clobber registers at will!
    push af
        call getCurrentThreadID
        call getThreadEntry
        inc hl \ inc hl \ inc hl
        ld c, (hl) \ inc hl \ ld b, (hl)
        push bc \ pop ix
        call memSeekToStart
        dec ix \ dec ix
        ld c, (ix) \ ld b, (ix + 1)
        add ix, bc
        ld l, (ix)
        ld h, (ix + 1)
    pop af
    jp (hl)

; Input:  A: Thread ID
; Output: HL: Thread entry
;; getThreadEntry [Threading]
;;  Gets a pointer to the specified thread's entry in the thread table.
;; Inputs:
;;  A: Thread ID
;; Outputs:
;;  HL: Thread entry
;; Notes:
;;  You must disable interrupts while manipulating the thread table to
;;  guarantee that it will not change while you do so.
getThreadEntry:
    push bc
        ld c, a
        ld b, maxThreads
        ld hl, threadTable
_:      ld a, (hl)
        cp c
        jr nz, _
        pop bc
        ret
_:      ld a, 8
        add a, l
        ld l, a
        djnz --_
    pop bc
    or 1
    ld a, errNoSuchThread
    ret

;; setReturnPoint [Threading]
;;  Sets the return point for the specified thread. This is set to
;;  [[killThread]] by default.
;; Inputs:
;;  A: Thread ID
;;  HL: Return point
;; Outputs:
;;  A: Error code (on failure)
;;  Z: Set if successful, reset otherwise
setReturnPoint:
    push de
    push bc
        ex de, hl
        call getThreadEntry
        jr z, _
        pop bc
        pop de
        ret
_:      inc hl \ inc hl \ inc hl
        ld c, (hl) \ inc hl \ ld b, (hl)
        push bc \ pop ix
        call memSeekToStart
        dec ix \ dec ix
        ld c, (ix) \ ld b, (ix + 1)
        add ix, bc
        ld (ix), e
        ld (ix + 1), d
    pop bc
    pop de
    ret

;; setInitialBC [Threading]
;;  Sets the initial value of the BC register for the specified thread.
;; Inputs:
;;  HL: Initial value of BC
;;  A: Thread ID
;; Outputs:
;;  A: Error code (on failure)
;;  Z: Set if successful, reset otherwise
;; Note:
;;  Do **not** call this function on a thread that has already been started.
;;  You must have interrupts disabled when you call [[startThread]], and
;;  leave them disabled until after you have finished setting the initial
;;  state.
setInitialBC:
    push hl
        push de
            ex de, hl
            call getThreadEntry
            jr z, _
        pop de
    pop hl
    ret
_:          inc hl \ inc hl \ inc hl
            push bc
                ld c, (hl) \ inc hl \ ld b, (hl)
                push bc \ pop ix
                call memSeekToStart
                dec ix \ dec ix
                ld c, (ix) \ ld b, (ix + 1)
                add ix, bc
            pop bc
            ld (ix + -6), e
            ld (ix + -5), d
        pop de
    pop hl
    ret

;; setInitialDE [Threading]
;;  Sets the initial value of the DE register for the specified thread.
;; Inputs:
;;  HL: Initial value of DE
;;  A: Thread ID
;; Outputs:
;;  A: Error code (on failure)
;;  Z: Set if successful, reset otherwise
;; Note:
;;  Do **not** call this function on a thread that has already been started.
;;  You must have interrupts disabled when you call [[startThread]], and
;;  leave them disabled until after you have finished setting the initial
;;  state.
setInitialDE:
    push hl
        push de
            ex de, hl
            call getThreadEntry
            jr z, _
        pop de
    pop hl
    ret
_:          inc hl \ inc hl \ inc hl
            push bc
                ld c, (hl) \ inc hl \ ld b, (hl)
                push bc \ pop ix
                call memSeekToStart
                dec ix \ dec ix
                ld c, (ix) \ ld b, (ix + 1)
                add ix, bc
            pop bc
            ld (ix + -8), e
            ld (ix + -7), d
        pop de
    pop hl
    ret

;; setInitialHL [Threading]
;;  Sets the initial value of the HL register for the specified thread.
;; Inputs:
;;  HL: Initial value of HL
;;  A: Thread ID
;; Outputs:
;;  A: Error code (on failure)
;;  Z: Set if successful, reset otherwise
;; Note:
;;  Do **not** call this function on a thread that has already been started.
;;  You must have interrupts disabled when you call [[startThread]], and
;;  leave them disabled until after you have finished setting the initial
;;  state.
setInitialHL:
    push hl
        push de
            ex de, hl
            call getThreadEntry
            jr z, _
        pop de
    pop hl
    ret
_:          inc hl \ inc hl \ inc hl
            push bc
                ld c, (hl) \ inc hl \ ld b, (hl)
                push bc \ pop ix
                call memSeekToStart
                dec ix \ dec ix
                ld c, (ix) \ ld b, (ix + 1)
                add ix, bc
            pop bc
            ld (ix + -10), e
            ld (ix + -9), d
        pop de
    pop hl
    ret

;; setInitialA [Threading]
;;  Sets the initial value of the A register for the specified thread.
;; Inputs:
;;  H: Initial value of A
;;  A: Thread ID
;; Outputs:
;;  A: Error code (on failure)
;;  Z: Set if successful, reset otherwise
;; Note:
;;  Do **not** call this function on a thread that has already been started.
;;  You must have interrupts disabled when you call [[startThread]], and
;;  leave them disabled until after you have finished setting the initial
;;  state.
setInitialA:
    push hl
        push de
            ex de, hl
            call getThreadEntry
            jr z, _
        pop de
    pop hl
    ret
_:          inc hl \ inc hl \ inc hl
            push bc
                ld c, (hl) \ inc hl \ ld b, (hl)
                push bc \ pop ix
                call memSeekToStart
                dec ix \ dec ix
                ld c, (ix) \ ld b, (ix + 1)
                add ix, bc
            pop bc
            ld (ix + -3), d
        pop de
    pop hl
    ret

;; setInitialIX [Threading]
;;  Sets the initial value of the IX register for the specified thread.
;; Inputs:
;;  HL: Initial value of IX
;;  A: Thread ID
;; Outputs:
;;  A: Error code (on failure)
;;  Z: Set if successful, reset otherwise
;; Note:
;;  Do **not** call this function on a thread that has already been started.
;;  You must have interrupts disabled when you call [[startThread]], and
;;  leave them disabled until after you have finished setting the initial
;;  state.
setInitialIX:
    push hl
        push de
            ex de, hl
            call getThreadEntry
            jr z, _
        pop de
    pop hl
    ret
_:          inc hl \ inc hl \ inc hl
            push bc
                ld c, (hl) \ inc hl \ ld b, (hl)
                push bc \ pop ix
                call memSeekToStart
                dec ix \ dec ix
                ld c, (ix) \ ld b, (ix + 1)
                add ix, bc
            pop bc
            ld (ix + -12), e
            ld (ix + -11), d
        pop de
    pop hl
    ret

;; setInitialIY [Threading]
;;  Sets the initial value of the IY register for the specified thread.
;; Inputs:
;;  HL: Initial value of IY
;;  A: Thread ID
;; Outputs:
;;  A: Error code (on failure)
;;  Z: Set if successful, reset otherwise
;; Note:
;;  Do **not** call this function on a thread that has already been started.
;;  You must have interrupts disabled when you call [[startThread]], and
;;  leave them disabled until after you have finished setting the initial
;;  state.
setInitialIY:
    push hl
        push de
            ex de, hl
            call getThreadEntry
            jr z, _
        pop de
    pop hl
    ret
_:  inc hl \ inc hl \ inc hl \ push bc
                ld c, (hl) \ inc hl \ ld b, (hl)
                push bc \ pop ix
                call memSeekToStart
                dec ix \ dec ix
                ld c, (ix) \ ld b, (ix + 1)
                add ix, bc
            pop bc
            ld (ix + -14), e
            ld (ix + -13), d
        pop de
    pop hl
    ret

; TODO: suspendThread
;; suspendCurrentThread [Threading]
;;  Suspends the currently executing thread.
;; Notes:
;;  This function will not return until a second thread resumes the
;;  current thread.
suspendCurrentThread:
    push hl
    push af
        call getCurrentThreadId
        call getThreadEntry
        ld a, 5
        add a, l
        ld l, a
        set 2, (hl)
        ei \ halt
    pop af
    pop hl
    ret

;; resumeThread [Threading]
;;  Resumes the specified thread.
;; Inputs:
;;  A: Thread ID
; TODO: Errors
resumeThread:
    push hl
    push af
        call getThreadEntry
        ld a, 5
        add a, l
        ld l, a
        res 2, (hl)
    pop af
    pop hl
    ret

