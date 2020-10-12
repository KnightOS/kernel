;; getLcdLock [Hardware]
;;  Locks the LCD to the current thread.
getLCDLock:
    push af
        call getCurrentThreadId
        ld (hwLockLCD), a
        call checkLegacyLcdMode
        jr nz, _
        call setLegacyLcdMode
_:  pop af
    ret

;; getIOLock [Hardware]
;;  Locks the I/O port to the current thread.
;;  Note that the I/O port can be used by several processes - it's usually best to
;;  leave it unlocked and let the kernel do your I/O.
getIOLock:
    push af
        call getCurrentThreadId
        ld (hwLockIO), a
    pop af
    ret

;; getKeypadLock [Hardware]
;;  Locks the keyboard to the current thread.
getKeypadLock:
    push af
        call getCurrentThreadId
        ld (hwLockKeypad), a

        ; Flush keys
        call flushKeys
_:      call getScanCode
        jr z, -_
    pop af
    ret

;; getUSBLock [Hardware]
;;  Locks the USB port to the current thread.
getUSBLock:
    push af
        call getCurrentThreadId
        ld (hwLockUSB), a
    pop af
    ret

;; hasLCDLock [Hardware]
;;  Sets Z if the current thread has a lock on the LCD.
hasLCDLock:
    push hl
    push af
        call getCurrentThreadId
        ld hl, hwLockLCD
        cp (hl)
    pop hl
    ld a, h
    pop hl
    ret

;; hasIOLock [Hardware]
;;  Sets Z if the current thread has a lock on the I/O port.
hasIOLock:
    push hl
    push af
        call getCurrentThreadId
        ld hl, hwLockIO
        cp (hl)
    pop hl
    ld a, h
    pop hl
    ret

;; hasKeypadLock [Hardware]
;;  Sets Z if the current thread has a lock on the keyboard.
hasKeypadLock:
    push hl
    push af
        call getCurrentThreadId
        ld hl, hwLockKeypad
        cp (hl)
    pop hl
    ld a, h
    pop hl
    ret

;; hasUSBLock [Hardware]
;;  Sets Z if the current thread has a lock on the USB.
hasUSBLock:
#ifdef USB
    push hl
    push af
        call getCurrentThreadId
        ld hl, hwLockUSB
        cp (hl)
    pop hl
    ld a, h
    pop hl
    ret
#else
    push bc
        ld b, a
        or a
        ld a, b
    pop bc
#endif
    ret

