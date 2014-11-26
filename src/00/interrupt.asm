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
    bit BIT_INT_TRIG_ON, a
    jr nz, intHandleON
    bit BIT_INT_TRIG_TIMER1, a
    jr nz, intHandleTimer1
    bit BIT_INT_TRIG_TIMER2, a
    jp nz, intHandleTimer2
    bit BIT_INT_TRIG_LINK, a
    jp nz, intHandleLink
#ifdef LINK_ASSIST
    in a, (PORT_LINKASSIST_STATUS)
    bit BIT_LINKASSIST_RECV_ONCOMPLETE, a
    jp nz, handleNewIOByte
    ld b, a
    ld a, (IOState)
    bit 5, a
    jr z, contextSwitch
    bit BIT_LINKASSIST_SEND_ONREADY, b
    jp nz, sendNewIOByte
#else
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
intHandleTimer1:
    in a, (PORT_INT_MASK)
    res BIT_INT_TIMER1, a
    out (PORT_INT_MASK), a
    set BIT_INT_TIMER1, a
    out (PORT_INT_MASK), a
    ; Timer 1 interrupt
doContextSwitch:
    ld a, (currentThreadIndex)
    add a, a
    add a, a
    add a, a
    ld hl, threadTable + 3
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
    ld a, (activeThreads)
    or a \ jr z, noThreads ; Error out when there are no active threads
    ld c, a \ inc c
contextSwitch_search:
    dec c
    xor a
    cp c
    jr z, noActiveThreads
    ld a, (currentThreadIndex)
    inc a \ ld (currentThreadIndex), a
    ld b, a
    ld a, (activeThreads)
    dec a \ cp b
    jr nc, _
    xor a
    ld b, a
    ld (currentThreadIndex), a
_:  ld a, b
    add a, a
    add a, a
    add a, a

    ld hl, threadTable + 5
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

    jp sysInterruptDone
noThreads:
    ld a, panic_no_threads
    jp panic
noActiveThreads:
    ld a, panic_no_active_threads
    set 7, a
    jp panic
intHandleTimer2:
    in a, (PORT_INT_MASK)
    res BIT_INT_TIMER2, a
    out (PORT_INT_MASK), a
    set BIT_INT_TIMER2, a
    out (PORT_INT_MASK), a
    ; Timer 2 interrupt
    jp sysInterruptDone

intHandleLink:
    in a, (PORT_INT_MASK)
    res BIT_INT_LINK, a
    out (PORT_INT_MASK), a
    set BIT_INT_LINK, a
    out (PORT_INT_MASK), a
    jp sysInterruptDone

#ifdef LINK_ASSIST
handleNewIOByte:
    in a, (PORT_LINKASSIST_OUTPUT) ; ACK interrupt at the same time
    ld (temp_io_var), a
    ld hl, IOstate
    ld a, IO_STATE_IDLE
    cp (hl)
    jr nz, .updateIORecv
    ; start a new reception
    ; enable the link assist's ready-to-send interruption
    ld a, LINKASSIST_INT_ONRECV | LINKASSIST_INT_ONREADY
    out (PORT_LINKASSIST_ENABLE), a
    ld (hl), IO_STATE_RECV | IO_STATE_PORTH ; we just received PORTL
    xor a
    ld (IOIsSending), a
    ld (IODataChecksum), a
    ld (IOTransferErrored), a
    ld a, (currentIOFrame)
    ld (busyIOFrame), a
    push af
        inc a
        cp maxIOFrames
        call nc, dropOldestIOFrame ; in connectivity.asm
        ld (currentIOFrame), a
    pop af
    add a, a
    ld c, a
    add a, a
    add a, c
    ld b, 0
    ld c, a
    ld hl, IOFramesQueue
    add hl, bc
    ld a, IOinFrame | IOFrameBusy
    ld (hl), a ; write header first
    inc hl
    ld a, (temp_io_var)
    ld (hl), a ; then write PORTL
    jp sysInterruptDone
.updateIORecv:
    ld a, (IOstate)
    bit 7, a
    jp nz, .handleRecvACKByte
    ld b, a
    ld a, (busyIOFrame)
    add a, a
    ld c, a
    add a, a
    add a, c
    ld c, a
    ld a, b
    ld b, 0
    ld hl, IOFramesQueue
    add hl, bc
    inc hl \ inc hl
    ld c, a
    ld a, (temp_io_var)
    ld e, a
    ld a, 0x1F
    and c
    cp IO_STATE_PORTH
    jr z, .saveIOByte
    inc hl
    cp IO_STATE_LEN
    jr z, .saveIOByte
    ; The remaining states are up to handleACKByte, so that means we got a new data byte in E
    ; Put frame length in C
    ld c, (hl)
    inc hl
    ld a, (currentIODataByte)
    or a
    jr nz, .updateIOData
    ; create a new data pointer
    ld b, 0
    call malloc
    ; ####
    ; TODO : handle when this fails
    ; ####
    push de \ push ix \ pop de
        ld (hl), e
        inc hl
        ld (hl), d
        dec hl
    pop de
.updateIOData:
    ; at this point :
    ; A: offset in data where the new byte goes
    ; C: total length of the data - complete transfer if A + 1 = C when done writing
    ; E: new data byte
    ; HL: pointer on pointer on data location
    ld b, a
    ; first, add new byte to checksum
    ld a, (IODataChecksum)
    add a, e
    ld (IODataChecksum), a
    ; then write the byte to data
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ld a, b
    add a, l \ ld l, a
    ld a, 0 \ adc a, h \ ld h, a
    ld (hl), e
    ld b, a
    inc a
    cp c
    jr z, .frameComplete
    ld (currentIODataByte), a
    jp sysInterruptDone
.frameComplete:
    ld a, IO_STATE_ACK | IO_STATE_RECV | IO_STATE_CHECKSUM
    ld (IOstate), a
    jp sysInterruptDone
.saveIOByte:
    ld (hl), e
    ld hl, IOstate
    inc (hl)
    jp sysInterruptDone
.handleRecvACKByte:
    ; A: IO state
    ; this executing means that we received a byte -> assume receive mode
    and 0x1F
    cp IO_STATE_CHECKSUM
    jr z, .handleRecvChecksum
    ; We got the queue length.
    ; First, see if the checksum was actually correct,
    ; if no, abort receiving the frame. The other calc will send it again next cycle.
    ld a, (IOTransferErrored)
    or a
    jp nz, abortBusyRecvFrame
    ; checksums matched, carry on
    ; see which of the queue lengths is the smallest
    call getQueueLength
    cp c
    sbc a, a
    jr z, .sameQueues
    ld (willSendNextIOFrame), a
    jr .doneWithQueues
.sameQueues:
    ld a, (IOIsSending)
    ld (willSendNextIOFrame), a
.doneWithQueues:
    ; if we're not the sender, send our own queue length
    ; if we are, that means we did it already, so we're done for this transfer !
    ld a, (IOIsSending)
    or a
    jr nz, +_
    ld a, IO_STATE_ACK | IO_STATE_SEND | IO_STATE_CHECKSUM
    jr ++_
_:
    ; disable the link assist's ready-to-send interruption
    ld a, LINKASSIST_INT_ONRECV
    out (PORT_LINKASSIST_ENABLE), a
    ld a, IO_STATE_IDLE
_:
    ld (IOstate), a
    jp sysInterruptDone
.handleRecvChecksum:
    ; We got the checksum byte.
    ; See if it matches our own.
    ld a, (temp_io_var)
    ld hl, IODataChecksum
    cp (hl)
    jr z, +_                       ; transfer contains errors !
    ld a, 1
    ld (IOTransferErrored), a      ; this will be handled when sending or receiving queue length
_:
    ; if we're not the sender, send our own checksum
    ; if we are, that means we did it already, so we move to sending our queue's length
    ld a, (IOIsSending)
    or a
    jr nz, +_
    ld a, IO_STATE_ACK | IO_STATE_SEND | IO_STATE_CHECKSUM
    jr ++_
_:
    ld a, IO_STATE_ACK | IO_STATE_SEND | IO_STATE_QUEUE
_:
    ld (IOstate), a
    jp sysInterruptDone
#else
    ; ####
    ; TODO : handle 73/83+ receive
    ; ####
#endif

#ifdef LINK_ASSIST
sendNewIOByte:
    ld a, (IOstate)
    bit 7, a
    jr nz, .handleSendACKByte
    ; ####
    ; TODO : send frames
    ; ####
    jp sysInterruptDone
.handleSendACKByte:
    ; A: IO state
    ; This executing means that we're sending a byte -> assume send mode.
    and 0x1F
    cp IO_STATE_CHECKSUM
    jr z, .handleSendChecksum
    ; Send queue length.
    ; First, see if the checksum was actually correct,
    ; if no, abort receiving the frame. The other calc will send it again next cycle.
    ld a, (IOTransferErrored)
    or a
    jr nz, abortBusyRecvFrame
    call getQueueLength
    ld a, c
    out (PORT_LINKASSIST_INPUT), a
    ; if we are the sender, receive the other's queue length
    ; if we're not, we're done with the transfer !
    ld a, (IOIsSending)
    or a
    jr z, +_
    ld a, IO_STATE_ACK | IO_STATE_RECV | IO_STATE_QUEUE
    jr ++_
_:
    ; disable the link assist's ready-to-send interruption
    ld a, LINKASSIST_INT_ONRECV
    out (PORT_LINKASSIST_ENABLE), a
    ld a, IO_STATE_IDLE
_:
    ld (IOstate), a
    jp sysInterruptDone
.handleSendChecksum:
    ; Send checksum. Literally.
    ld a, (IODataChecksum)
    out (PORT_LINKASSIST_INPUT), a
    jp sysInterruptDone
#else

#endif

abortBusyRecvFrame:
    ld a, (busyIOFrame)
    add a, a
    ld c, a
    add a, a
    add a, c
    ld hl, IOFramesQueue
    ld c, a
    ld b, 0
    add hl, bc
    ld (hl), IOinactive
    inc hl \ inc hl
    inc hl
    push hl \ pop ix
    call free
#ifdef LINK_ASSIST
    ; disable the link assist's ready-to-send interruption
    ld a, LINKASSIST_INT_ONRECV
    out (PORT_LINKASSIST_ENABLE), a
#endif
    jr sysInterruptDone

; Returns the number of ready-to-send frames in BC
; /!\ only preserves A /!\
getQueueLength:
    ld bc, (8 << 8) + 0
    ld de, 6
    ld hl, IOFramesQueue
.searchLoop:
    bit 1, (hl)
    jr z, +_
    inc c
_:
    add hl, de
    djnz .searchLoop
    ret
    
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
    ld a, (hwLockLCD)
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
