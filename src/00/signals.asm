; KnightOS Signal Management
; For handling communication between threads

; Inputs:   A:  Target thread ID
;           B:  Message type
;           HL: Message payload
; Adds a signal to the signal queue
;; createSignal [Threading]
;;  Signals another thread with a simple message.
;; Inputs:
;;  A: Target thread ID
;;  B: Message type
;;  HL: Message payload
;; Notes:
;;  The receiving thread may use [[readSignal]] to consume this
;;  message.
createSignal:
    push af
    push hl
    push de
    ld d, a ; Save thread ID
    ld a, i
    push af
        ld a, d
        push af
            ex de, hl
            ld a, (activeSignals)
            cp maxSignals
            jr nc, createSignal_tooMany
            add a, a \ add a, a
            ld hl, signalTable
            add a, l
            ld l, a
            jr nc, $+3 \ inc h
            ; HL points to target signal address
        pop af
        ld (hl), a \ inc hl \ ld (hl), b \ inc hl
        ld (hl), e \ inc hl \ ld (hl), d
        ld hl, activeSignals
        inc (hl)
    pop af
    jp po, _
    ei
_:  pop de
    pop hl
    pop af
    cp a
    ret
createSignal_tooMany:
    pop af
    pop af
    pop de
    pop hl
    jp po, _
    ei
_:  pop af
    or 1
    ld a, errTooManySignals
    ret

; Outputs:  NZ: No signals to read, or Z: Signal read, and:
;           B:  Message type
;           HL: Message payload
; Reads the next signal for the current thread.
;; readSignal [Threading]
;;  Reads the next pending signal from this thread's signal queue.
;; Outputs:
;;  Z: Set if a signal was read, reset if there are no pending signals
;;  B: Message type
;;  HL: Message payload
readSignal:
    push af
        call getCurrentThreadId
        call readSignalAsThread
    pop af
    ret

; Reads a signal that was originally sent to a thread whose
; ID should be given in A. This routine is meant for internal
; use (specifically, the thread killing code uses it to clear
; undelivered signals). Not for userland use.
readSignalAsThread:
    push hl
    push af
    ld a, i
    push af
    push bc
    push de
        ld de, 4
        ld l, a
        ld a, (activeSignals)
        or a
        jr z, readSignal_none
        ld b, a
        ld a, l
        ld hl, signalTable
_:      cp (hl)
        jr z, readSignal_found
        add hl, de
        djnz -_

readSignal_none:
    ; We don't want to destroy anything if it isn't found
    pop de
    pop bc
    pop af
    jp po, _
    ei
_:  pop af
    ld l, a
    or 1
    ld a, l
    pop hl
    ret
    
readSignal_found:
    inc hl \ ld a, (hl)
    inc hl \ ld e, (hl)
    inc hl \ ld d, (hl)
    ; Push values to return
    push af
    push de
        ; Remove signal
        dec hl \ dec hl \ dec hl
        ld d, h \ ld e, l
        ld bc, 4 \ add hl, bc
        ld a, (activeSignals)
        dec a ; Note: this will copy more than needed, but it isn't a problem
        ld (activeSignals), a
        jr z, _
        add a, a \ add a, a
        ld c, a \ ld b, 0
        ldir
_:  pop de
    pop af

    ex de, hl
    pop de
    pop bc
    ld b, a
    pop af
    jp po, _
    ei
_:  pop af
    inc sp \ inc sp ; pop hl
    cp a
    ret
    
