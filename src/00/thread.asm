initMultitasking:
    ld a, threadRangeMask ; When the first thread is allocated, this will wrap to 0
    ld (lastThreadId), a
    ld hl, threadTable
    ld (hl), 0
    ld de, threadTable + 1
    ld bc, threadTableSize
    ldir
    ret

; Returns the ID of the thread that will launch next
getNewThreadID:
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
        add a, a
        add a, a
        add a, a
        ld h, 0x80
        ld l, a
        ld a, (hl)
    pop hl
    ret

; setCurrentThread [Threading]
;  Sets the current thread context to the given ID
; Inputs:
;  A: Thread ID
setCurrentThread:
    push hl
    push bc
        ld c, a
        ld b, 0
        ld hl, threadTable
.loop:
        ld a, c
        cp (hl)
        jr z, .done
        inc b
        ld a, 8
        add a, l \ ld l, a \ jr nc, $+3 \ inc h
        jr .loop
.done:
        ld a, b
        ld (currentThreadIndex), a
    pop bc
    pop hl
    ret

;; checkThread [Threading]
;;  Checks to see if the specified thread ID is still running.
;; Inputs:
;;  A: Thread ID
;; Outputs:
;;  Z: Set if running, reset if not found
checkThread:
    push hl
        call getThreadEntry
    pop hl
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
            call getNewThreadID
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
                    add a, 64 ; Required minimum stack size for system use
                    ld c, a
                    jr nc, $+3 \ inc b
                    ld a, 1
                    call calloc
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
;;  allocated memory, loaded libraries, file handles, etc.
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
    jp contextSwitch_manual

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
;;  new thread for it. The file must be a valid KEXC executable.
;; Inputs:
;;  DE: Path to executable file
;; Outputs:
;;  A: Thread ID (on success), error code (on failure)
;;  Z: Set if successful, reset otherwise
;; Notes:
;;  Call this with interrupts disabled if you wish to manipulate the thread
;;  before it starts (for example, to set the initial value of the registers).
;;  See [[startThread]] for details.
launchProgram:
    push bc
    ld a, i
    push af
    di
    push hl
    push de
    push ix
        call openFileRead
        jp nz, .error

        call getStreamInfo
        ; TODO: If E > 0, then the file is too large. Error out before we ask malloc about it.
        call malloc
        jp nz, .error_pop2
        call getNewThreadId
        call reassignMemory

        call streamReadToEnd ; Read entire file into memory
        call closeStream
        
        ; Check magic number
        ld a, 'K'
        cp (ix)
        jr nz, .magic_error
        ld a, 'E'
        cp (ix + 1)
        jr nz, .magic_error
        ld a, 'X'
        cp (ix + 2)
        jr nz, .magic_error
        ld a, 'C'
        cp (ix + 3)
        jr nz, .magic_error

        ; Check minimum required kernel version, if present
        ld b, KEXC_KERNEL_VER
        push ix \ call _getThreadHeader \ pop ix
        jr nz, .no_minimum_ver
        ex hl, de
        
        call getKernelMajorVersion
        jr nz, .unknown_ver
        ; If running minimum ver is less than required, abort load
        ; TODO on major ver: decide what to do if major ver is greater
        ld a, e
        cp l
        jr c, .kernel_too_low

        call getKernelMinorVersion
        jr nz, .unknown_ver
        ld a, d
        cp l
        jr c, .kernel_too_low
        ; Running version meets requirements

; Running version is unknown
.unknown_ver:
; no minimum version is specified by the executable
.no_minimum_ver:
; Check for a relocation table
        ld b, KEXC_RELOCATION_TABLE
        push ix \ call _getThreadHeader \ pop ix
        call z, .relocate

        ; Grab header info
        ld b, KEXC_ENTRY_POINT
        push ix \ call _getThreadHeader \ pop ix
        jr nz, .no_entry_point
        push hl
            ; b still has KEXC_ENTRY_POINT, and KEXC_STACK_SIZE is 1 higher
            inc b
            push ix \ call _getThreadHeader \ pop ix
            ld c, l ; TODO: Error out if H is nonzero?
            jr z, _
            ld c, DEFAULT_STACK_SIZE

_:          ld b, KEXC_THREAD_FLAGS
            push ix \ call _getThreadHeader \ pop ix
            ld a, l
            jr z, _
            xor a ; Default flags
_:          ld b, c
            push ix \ pop hl
            call startThread
            jr nz, .error
        pop hl
        call setEntryPoint
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
.magic_error:
    ld a, errNoMagic
    jr .error
.no_entry_point:
    ld a, errNoEntryPoint
    jr .error
.kernel_too_low:
    ld a, errKernelMismatch
    jr .error
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
; thrashes de, bc, and hl
.relocate:
; ix = executable address
; hl = program-relative relocation table address
    push ix \ pop de
    add hl, de
; hl = absolute address of relocation table
.relocation_loop:
    ld e, (hl)
    inc hl
    ld d, (hl)
    ; de = first entry in relocation table
    dec hl
    ; hl: preserved
    ld bc, 0
    call cpBCDE
    ret z
    ; de contains the program-relative address of a program-relative pointer to relocate
    ; need to execute, in effect, `add (ix + de), ix`
    push ix
        add ix, de
        push ix \ pop de
    pop ix
    ; de = absolute address of pointer to relocate
   
    ; add (de), ix
    push ix \ pop bc
    ld a, (de)
    add a, c
    ld (de), a
    inc de
    ld a, (de)
    add a, b
    ld (de), a
    inc hl \ inc hl
    jr .relocation_loop

;; exitThread [Threading]
;;  Immediately terminates the running thread.
;; Notes:
;;  This is preferred to [[killCurrentThread]], since it will go through the caller-set
;;  exit function. This is often [[killCurrentThread]] anyway, but it may be set to a
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

;; getEntryPoint [Threading]
;;  Retrieves the entry point for the specified thread.
;; Inputs:
;;  A: Thread ID
;; Outputs:
;;  HL: Entry point
;; Notes:
;;  This gives you the location of the executable in memory. This does NOT
;;  get the KEXC entry point (KEXC_ENTRY_POINT) from the executable header.
;;  For that, use [[getHeaderValue]].
getEntryPoint:
    call getThreadEntry
    ret nz
    push de
        inc hl
        ld e, (hl)
        inc hl
        ld d, (hl)
        ex de, hl
    pop de
    ret

; getThreadEntry [Threading]
;  Gets a pointer to the specified thread's entry in the thread table.
; Inputs:
;  A: Thread ID
; Outputs:
;  HL: Thread entry
; Notes:
;  You must disable interrupts while manipulating the thread table to
;  guarantee that it will not change while you do so.
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

;; getHeaderValue [Threading]
;;  Finds a header in the specified thread, and returns its value.
;; Inputs:
;;  A: Thread ID
;;  B: Header
;; Outputs:
;;  Z: Set if found, reset if not
;;  A: Preserved unless error
;;  HL: Header value
;; Notes:
;;  This only works for threads that are valid KEXC formatted executables.
;;  This fails for threads manually created through other means.
getThreadHeader:
    push ix
        call getThreadEntry
        inc hl
        push bc
            ld c, (hl)
            inc hl
            ld b, (hl)
            ld ixh, b \ ld ixl, c
        pop bc
        push af
            call _getThreadHeader
            jr nz, _
        pop af
    pop ix
    ret
_:      cp errNoMagic
        jr z, _
        pop af
    pop ix
    or 1
    ld a, errNoHeader
    ret
_:      pop af
    pop ix
    or 1
    ld a, errNoMagic
    ret

; IX: Thread in memory, other inputs are the same as getHeaderValue
_getThreadHeader:
    ld a, 'K'
    cp (ix)
    jr nz, .magic_error
    inc ix
    ld a, 'E'
    cp (ix)
    jr nz, .magic_error
    inc ix
    ld a, 'X'
    cp (ix)
    jr nz, .magic_error
    inc ix
    ld a, 'C'
    cp (ix)
    jr nz, .magic_error
    inc ix
    ; Actually do the check
    push ix \ pop hl
_:  xor a
    cp (hl)
    jr z, .end_of_headers
    ld a, b
    cp (hl)
    jr z, .header_found
    inc hl \ inc hl \ inc hl
    jr -_
.end_of_headers:
    or 1
    ret
.header_found:
    push de
        inc hl
        ld e, (hl)
        inc hl
        ld d, (hl)
        ld h, d \ ld l, e
    pop de
    cp a
    ret
.magic_error:
    or 1
    ld a, errNoMagic
    ret

; Intentionally not exposed to userspace
setEntryPoint:
    push hl
    push de
    push bc
        ex de, hl
        call getThreadEntry
        jr nz, _
        inc hl
        ld c, (hl)
        inc hl
        ld b, (hl)
        dec hl \ dec hl
        or a
        ex de, hl
        adc hl, bc
        ex de, hl
        call sharedSetInitial
        ld (ix + -2), e
        ld (ix + -1), d
_:  pop bc
    pop de
    pop hl
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
    push hl
    push ix
    push de
        ex de, hl
        call getThreadEntry
        jr nz, _
        call sharedSetInitial
        ld (ix), e
        ld (ix + 1), d
_:  pop de
    pop ix
    pop hl
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
            jr nz, _
            call sharedSetInitial
            ld (ix + -6), e
            ld (ix + -5), d
_:      pop de
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
            jr nz, _
            call sharedSetInitial
            ld (ix + -8), e
            ld (ix + -7), d
_:      pop de
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
            jr nz, _
            call sharedSetInitial
            ld (ix + -10), e
            ld (ix + -9), d
_:      pop de
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
            jr nz, _
            call sharedSetInitial
            ld (ix + -3), d
_:      pop de
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
            jr nz, _
            call sharedSetInitial
            ld (ix + -12), e
            ld (ix + -11), d
_:      pop de
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
            jr nz, _
            call sharedSetInitial
            ld (ix + -14), e
            ld (ix + -13), d
_:      pop de
    pop hl
    ret

; shared thread access call for the setInitial** funcs
; do NOT call this in your code, never !!!
sharedSetInitial:
    inc hl \ inc hl \ inc hl
    push bc
        ld c, (hl) \ inc hl \ ld b, (hl)
        push bc \ pop ix
        call memSeekToStart
        dec ix \ dec ix
        ld c, (ix) \ ld b, (ix + 1)
        add ix, bc
    pop bc
    ret

; TODO: suspendThread
;; suspendCurrentThread [Threading]
;;  Suspends the currently executing thread.
;; Notes:
;;  This function will not return until a second thread resumes the current thread.
suspendCurrentThread:
    push hl
    push af
        call getCurrentThreadId
        call getThreadEntry
        ld a, 5
        add a, l
        ld l, a
        set 2, (hl)
        call contextSwitch
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

;; getNextThreadID [Threading]
;;  Gets the next running thread's ID.
;; Inputs:
;;  B: Thread index (0 for first thread)
;; Outputs:
;;  A: Thread ID or 0xFF if no more threads
;;  B: Incremented
;; Notes:
;;  It would be silly to call this without interrupts disabled.
;;
;;  This will return your own thread at some point.
getNextThreadID:
    ld a, b
    and threadRangeMask
    cp b
    jr nz, .fail
    sla a
    sla a
    sla a
    push de
    push hl
        ld d, 0
        ld e, a
        ld hl, threadTable
        add hl, de
        ld a, (hl)
    pop hl
    pop de
    or a
    jr z, .fail
    inc b
    ret

.fail:
    ld a, 0xFF
    inc b
    ret
