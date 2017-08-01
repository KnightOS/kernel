;; contextSwitch [System]
;;  Triggers a context switch early. This will transfer control from your thread to
;;  another and eventaully return to yours with interrupts enabled.
contextSwitch:
    di
    push af
    push bc
    push de
    push hl
    push ix
    push iy
    exx
    ex af, af'
    push af
    push bc
    push de
    push hl
    jr doContextSwitch

sysInterrupt:
    di
    push af
    push bc
    push de
    push hl
    push ix
    push iy
    exx
    ex af, af'
    push af
    push bc
    push de
    push hl

#ifdef USB
    jp usbInterrupt
interruptResume:
#endif

    in a, (PORT_INT_TRIG)
#ifdef CRYSTAL_TIMERS
    bit BIT_INT_TRIG_CRYS1, a
    jp nz, io_timer_expired
    bit BIT_INT_TRIG_CRYS2, a
    jp nz, intHandleCrys2
    bit BIT_INT_TRIG_CRYS3, a
    jp nz, intHandleCrys3
#endif
    bit BIT_INT_TRIG_ON, a
    jp nz, intHandleON
    bit BIT_INT_TRIG_TIMER1, a
    jp nz, intHandleTimer1
    bit BIT_INT_TRIG_TIMER2, a
    jp nz, intHandleTimer2
    bit BIT_INT_TRIG_LINK, a
    jp nz, intHandleLink

#ifdef LINK_ASSIST
    in a, (PORT_LINK_ASSIST_STATUS)
    and 0b00000111 ; LA_INT_* bits
    jp nz, la_handleInterrupt
#endif
#ifdef LINK_ASSIST_RO
    ; TODO
#endif

    jr contextSwitch
intHandleON:
    in a, (PORT_INT_MASK)
    res BIT_INT_ON, a
    out (PORT_INT_MASK), a
    set BIT_INT_ON, a
    out (PORT_INT_MASK), a

    ; Check for special keycodes
    jp handleKeyboard
intHandleCrys2:
    ld c, PORT_CRYS2_FREQ
    jr _
intHandleCrys3:
    ld c, PORT_CRYS3_FREQ
_:  xor a
    out (c), a
    inc c ; LOOP port for interrupt ACK
    in a, (c)
    out (c), a
    jp sysInterruptDone
intHandleTimer1:
    in a, (PORT_INT_MASK)
    res BIT_INT_TIMER1, a
    out (PORT_INT_MASK), a
    set BIT_INT_TIMER1, a
    out (PORT_INT_MASK), a
    ; Timer 1 interrupt
    ld hl, (kernel_current_time)
    inc hl
    ld (kernel_current_time), hl
doContextSwitch:
    ld a, (current_thread_index)
    cp -1
    jr z, contextSwitch_manual
    add a, a
    add a, a
    add a, a
    ld hl, thread_table + 3
    add a, l
    ld l, a
    ex de, hl
        ld hl, 0
        add hl, sp
    ex de, hl
    ; Save stack pointer
    ld (hl), e
    inc hl
    ld (hl), d

contextSwitch_manual:
    ld a, (active_threads)
    or a \ jr z, noThreads ; Error out when there are no active threads
    ld c, a \ inc c
contextSwitch_search:
    dec c
    xor a
    cp c
    jr z, noactive_threads
    ld a, (current_thread_index)
    inc a \ ld (current_thread_index), a
    ld b, a
    ld a, (active_threads)
    dec a \ cp b
    jr nc, _
    xor a
    ld b, a
    ld (current_thread_index), a
_:  ld a, b
    add a, a
    add a, a
    add a, a

    ld hl, thread_table + 5
    add a, l
    ld l, a
    ld a, (hl)
    bit 1, a ; May be suspended
    jr nz, _
    bit 2, a
    jr nz, contextSwitch_search ; Suspended

_:  dec hl
    ld d, (hl)
    dec hl
    ld e, (hl)
    ex de, hl
    ld sp, hl

    jr sysInterruptDone
noThreads:
    jr sysInterruptDone
    ld a, panic_no_threads
    jp panic
noactive_threads:
    ld a, -1
    ld (current_thread_index), a
    ld hl, idlethread_stack
    ld de, heap - (idlethread_stack_end - idlethread_stack)
    ld bc, idlethread_stack_end - idlethread_stack
    ldir
    ld sp, heap - (idlethread_stack_end - idlethread_stack)
    jr sysInterruptDone
intHandleTimer2:
    in a, (PORT_INT_MASK)
    res BIT_INT_TIMER2, a
    out (PORT_INT_MASK), a
    set BIT_INT_TIMER2, a
    out (PORT_INT_MASK), a
    ; Timer 2 interrupt
    jr sysInterruptDone

intHandleLink:
    in a, (PORT_INT_MASK)
    res BIT_INT_LINK, a
    out (PORT_INT_MASK), a
    set BIT_INT_LINK, a
    out (PORT_INT_MASK), a
    ; Link interrupt
sysInterruptDone:
    pop hl
    pop de
    pop bc
    pop af
    exx
    ex af, af'
    pop iy
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ei
    ret

; Sits and spins if all other threads are suspended
idlethread:
    jr idlethread
idlethread_stack:
    .dw 0
    .dw 0
    .dw 0
    .dw 0
    .dw 0
    .dw 0
    .dw 0
    .dw 0
    .dw 0
    .dw 0
    .dw idlethread
idlethread_stack_end:

handleKeyboard:
    call getKey_skipCheck
    call flushKeys_skipCheck
    cp kK
    jr z, handleOnK
    cp kR
    jp z, reboot
    cp kMODE
    jr z, handleOnMODE
#ifdef DEBUG
    cp kPRGM
    jr z, handleOnPRGM
#endif
    jr sysInterruptDone

handleOnMODE:
    ld de, init
    call launchProgram
    ld h, 1
    call setInitialA
    jr sysInterruptDone

handleOnK:
    ld a, (hw_lock_lcd)
    call killThread
    jp contextSwitch_manual

#ifdef DEBUG
handleOnPRGM:
    rst 0x30
    jp sysInterruptDone
#endif

#ifdef USB
usbInterrupt:
    in a, (0x55) ; USB Interrupt status
    bit BIT_USB_INT_BUS, a
    jr z, usbUnknownEvent
    bit BIT_USB_INT_LINE, a
    jr z, usbLineEvent
    bit BIT_USB_INT_PROTOCOL, a
    jr z, usbProtocolEvent
    jp interruptResume

usbUnknownEvent:
    jp interruptResume

usbLineEvent:
    in a, (PORT_USB_LINE) ; USB Line Events
    xor 0xFF
    out (PORT_USB_LINE_MASK), a ; Acknowledge interrupt and disable further interrupts
    jp interruptResume

usbProtocolEvent:
    in a, (PORT_USB_WRPIPE1)
    in a, (PORT_USB_WRPIPE2)
    in a, (PORT_USB_RDPIPE1)
    in a, (PORT_USB_RDPIPE2)
    in a, (PORT_USB_MISC_EVENTS) ; Merely reading from these will acknowledge the interrupt
    jp interruptResume
#endif
