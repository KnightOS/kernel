.macro ACKReach()
    push bc \ push de \ push hl \ push iy
        ld iy, 0xfc00
        call clearBuffer
        ld hl, 0xfc00
        ld de, 0xfc01
        ld bc, 0x2ff
        ld (hl), 0xff
        ldir
        call fastCopy_skipCheck
    pop iy \ pop hl \ pop de \ pop bc
.endmacro

;; contextSwitch [System]
;;  Triggers a context switch early. This will transfer control from your thread to
;;  another and eventually return to yours with interrupts enabled.
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
    jp doContextSwitch

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
    ; Handle reception here.
    ; Sending is taken care of when timer 1 completes.
#ifdef LINK_ASSIST
    in a, (PORT_LINKASSIST_STATUS)
    bit BIT_LINKASSIST_RECV_ONCOMPLETE, a
    jp nz, handleNewIOByte
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
    ; Handle IO send.
    ld a, (IOstate)
    cp IO_STATE_IDLE
    jr nz, .notIDLE
    ; no transfer is ongoing, search for awaiting ones
    ld hl, IOFramesQueue
    ld b, maxIOFrames
    ld c, 0
    ld de, 6
_:
    ld a, (hl)
    cp IOoutFrame
    jr z, .readyOutFrame
    add hl, de
    inc c
    djnz -_
    jr .sendDoneHandling
.readyOutFrame:
    ; we have a transfer that is ready to send, initialize it
    ld a, (hl)
    or IOFrameBusy
    ld (hl), a
    ld a, c
    ld (busyIOFrame), a
    ld a, IO_STATE_SEND | IO_STATE_PORTL
    ld (IOstate), a
    xor a
    ld (currentIODataByte), a
    ld (IODataChecksum), a
    ld (IOTransferErrored), a
    ld (IOSendConfirmed), a
    inc a
    ld (IOIsSending), a
    ; start transfer right away
    jp sendNewIOByte
.notIDLE:
    bit BIT_IO_STATE_SEND, a
    jr z, .sendDoneHandling
    in a, (PORT_LINKASSIST_STATUS)
    bit BIT_LINKASSIST_SEND_ISBUSY, a
    jr nz, .sendDoneHandling
    ; At this point, the IO state is in send mode and the link assist is not busy.
    ld a, (IOSendConfirmed)
    or a
    jp z, sendNewIOByte
    ; At this point, a send already completed
    xor a
    ld (IOSendConfirmed), a
.sendDoneHandling:
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
    ld (hl), IO_STATE_RECV | IO_STATE_PORTH ; we just received PORTL
    xor a
    ld (IOIsSending), a
    ld (IODataChecksum), a
    ld (IOTransferErrored), a
    ld (currentIODataByte), a
    ld a, (currentIOFrame)
    ld (busyIOFrame), a
    push af
        call dropNextIOFrame ; in connectivity.asm
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
    ld (hl), IOinFrame | IOFrameBusy ; write header first
    inc hl
    ld a, (temp_io_var)
    ld (hl), a ; then write PORTL
    jp sysInterruptDone
.updateIORecv:
    ld a, (IOstate)
    bit BIT_IO_STATE_ACK, a
    jp nz, .handleRecvACKByte
    ld a, (busyIOFrame)
    add a, a
    ld c, a
    add a, a
    add a, c
    ld c, a
    ld b, 0
    ld hl, IOFramesQueue
    add hl, bc
    inc hl \ inc hl
    ld a, (IOstate)
    and 0x1F
    cp IO_STATE_PORTH
    jp z, .saveIOByte
    inc hl
    cp IO_STATE_LEN
    jp z, .saveIOByte
    ; The remaining states are up to handleACKByte, so that means we got a new data byte.
    ; Put frame length in C.
    ld c, (hl)
    inc hl
    ld a, (currentIODataByte)
    or a
    jr nz, .updateIOData
    ; Create a new data pointer.
    ld b, 0
    call malloc
    ; ####
    ; TODO : handle when this fails
    ; ####
    ld b, a
    ld a, ixl
    ld (hl), a
    inc hl
    ld a, ixh
    ld (hl), a
    dec hl
    ld a, b
.updateIOData:
    ; at this point :
    ; A: offset in data where the new byte goes
    ; C: total length of the data - end data reception if A + 1 = C when done writing
    ; (temp_io_var): new data byte
    ; HL: pointer on pointer on data location
    ld b, a
    ; first, add new byte to checksum
    ld a, (temp_io_var)
    ld e, a
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
    ld a, b
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
    ld a, (temp_io_var)
    ld (hl), a
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
    ; if no, abort receiving the frame. The other calc will send it again the next cycle.
    ld a, (IOTransferErrored)
    or a
    jp nz, abortBusyRecvFrame
    ; checksums matched, carry on
    ; see which of the queue lengths is the smallest
    ld a, (temp_io_var)
    call getQueueLength
    cp c
    jr z, .sameQueues
    sbc a, a
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
    jr nz, .transferComplete
    ld a, IO_STATE_ACK | IO_STATE_SEND | IO_STATE_QUEUE
    jr .updateIOstate1
.transferComplete:
    ld a, IO_STATE_IDLE
.updateIOstate1:
    ld (IOstate), a
    jp sysInterruptDone
.handleRecvChecksum:
    ; We got the checksum byte.
    ; See if it matches our own.
    ld a, (temp_io_var)
    ld hl, IODataChecksum
    cp (hl)
    jr z, .checksumDone            ; NZ : transfer contains errors !
    ld a, 1
    ld (IOTransferErrored), a      ; this will be handled when sending or receiving queue length
.checksumDone:
    ; if we're not the sender, send our own checksum
    ; if we are, that means we did it already, so we move to sending our queue's length
    ld a, (IOIsSending)
    or a
    jr nz, .calcSends
    ld a, IO_STATE_ACK | IO_STATE_SEND | IO_STATE_CHECKSUM
    jr .updateIOstate2
.calcSends:
    ld a, IO_STATE_ACK | IO_STATE_SEND | IO_STATE_QUEUE
.updateIOstate2:
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
    bit BIT_IO_STATE_ACK, a
    jr nz, .handleSendACKByte
    ; We're not sending ACK bytes, meaning we're at the start or in the middle of an output
    ; First, grab the correct frame
    ld a, (busyIOFrame)
    add a, a
    ld c, a
    add a, a
    add a, c
    ld c, a
    ld b, 0
    ld hl, IOFramesQueue
    add hl, bc
    inc hl ; skip header
    ld a, (IOstate)
    and 0x1f
    ; Send bytes normally until reaching the data part
    cp IO_STATE_PORTL
    jr z, .sendByte
    inc hl
    cp IO_STATE_PORTH
    jr z, .sendByte
    inc hl
    cp IO_STATE_LEN
    jr z, .sendByte
    ld c, (hl) ; keep the data length in C
    inc hl
    ; The other states are up to handleSendACKByte, so that means we're sending data bytes
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ; Send bytes until currentIODataByte = C, while updating the checksum
    ld a, (currentIODataByte)
    ld e, a
    ld d, 0
    add hl, de
    ld a, (hl)
    out (PORT_LINKASSIST_INPUT), a
    ld hl, IODataChecksum
    add a, (hl)
    ld (hl), a
    ld a, c
    inc e
    cp e
    jr z, .sendDone
    ld a, e
    ld (currentIODataByte), a
    jp sysInterruptDone
.sendDone:
    ld a, IO_STATE_ACK | IO_STATE_SEND | IO_STATE_CHECKSUM
    ld (IOstate), a
    xor a
    ld (currentIODataByte), a
    jp sysInterruptDone
.sendByte:
    ld a, (hl)
    out (PORT_LINKASSIST_INPUT), a
    ld hl, IOstate
    inc (hl)
    jp sysInterruptDone
.handleSendACKByte:
    ; A: IO state
    ; This executing means that we're sending a byte -> assume send mode.
    and 0x1F
    cp IO_STATE_CHECKSUM
    jr z, .handleSendChecksum
    ; Send queue length.
    ; First, see if the checksum was actually correct ;
    ; if no, abort receiving the frame. The other calc will send it again next cycle.
    ld a, (IOTransferErrored)
    or a
    jr nz, abortBusyRecvFrame
    call getQueueLength
    ld a, c
    out (PORT_LINKASSIST_INPUT), a
    ; If we are the sender, receive the other's queue length.
    ; If we're not, we're done with the transfer !
    ld a, (IOIsSending)
    or a
    jr z, .transferComplete
    ld a, IO_STATE_ACK | IO_STATE_RECV | IO_STATE_QUEUE
    jr .updateIOstate3
.transferComplete:
    ; terminate reception
    ld a, (busyIOFrame)
    add a, a
    ld c, a
    add a, a
    add a, c
    ld hl, IOFramesQueue
    ld c, a
    ld b, 0
    add hl, bc
    res BIT_IOFrameBusy, (hl)
    xor a
    out (PORT_LINKPORT), a
    ld a, IO_STATE_IDLE
.updateIOstate3:
    ld (IOstate), a
    jp sysInterruptDone
.handleSendChecksum:
    ; Send checksum.
.waitForSend:
    in a, (PORT_LINKASSIST_STATUS)
    bit BIT_LINKASSIST_COM_ERRORED, a
    call nz, boot
    bit BIT_LINKASSIST_SEND_ISREADY, a
    jr z, .waitForSend
    ld a, (IODataChecksum)
    out (PORT_LINKASSIST_INPUT), a
    ld a, 1
    ld (IOSendConfirmed), a
    ; If we are the sender, receive the other's checksum.
    ; If we're not, we move to receiving the other's queue's length.
    ld a, (IOIsSending)
    or a
    jr nz, .calcSends2
    ld a, IO_STATE_ACK | IO_STATE_RECV | IO_STATE_QUEUE
    jr .updateIOstate4
.calcSends2:
    ld a, IO_STATE_ACK | IO_STATE_RECV | IO_STATE_CHECKSUM
.updateIOstate4:
    ld (IOstate), a
    jp sysInterruptDone
#else
    ; ####
    ; TODO : handle 73/83+ send
    ; ####
#endif

abortBusyRecvFrame:
    ld a, (busyIOFrame)
    dec a
    call dropNextIOFrame ; in connectivity.asm
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
    in a, (PORT_USB_INT)
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
