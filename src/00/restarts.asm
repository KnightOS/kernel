; rst 0x08
kcall:
    push hl
    inc sp \ inc sp
    pop hl
    push hl
    dec sp \ dec sp
    push de
    push bc
    push af

    ; HL has return address, stack is intact
    dec hl
    ld (hl), 0
    inc hl

    ld a, (hl)
    cp 0xDD
    jr z, _
    cp 0xFD
    jr z, _
    cp 0xED
    jr nz, ++_
_:
    inc hl ; Handle IX/IY prefix
_:
    inc hl

    ld c, (hl)
    inc hl
    ld b, (hl)
    dec hl

    push hl
        ld hl, thread_table + 1
        ld a, (current_thread_index)
        add a, a
        add a, a
        add a, a
        add a, l
        ld l, a
        jr nc, $+3
        inc h

        ld e, (hl)
        inc hl
        ld d, (hl)
    pop hl

    ex de, hl
    add hl, bc
    ex de, hl

    ld (hl), e
    inc hl
    ld (hl), d

    pop af
    pop bc
    pop de
    pop hl
    ret

; rst 0x10
lcall:
    push hl
    inc sp \ inc sp
    pop hl
    push hl
    dec sp \ dec sp
    push de
    push bc
    push af
        dec hl
        ld (hl), 0
        inc hl

        ld a, (hl)
        ld (hl), 0
        ld c, a
        inc hl
        ex de, hl
        ld hl, library_table
        ld a, (loaded_libraries)
        ld b, a
lmacro_SearchLoop:
        ld a, (hl)
        cp c
        jr z, _
        inc hl \ inc hl \ inc hl \ inc hl
        djnz lmacro_SearchLoop
        ld a, panic_library_not_found
        jp panic

_:      inc hl
        ld c, (hl)
        inc hl
        ld b, (hl)
        
        ex de, hl

        ld a, (hl) ; Handle IX/IY cases
        cp 0xDD
        jr z, _
        cp 0xFD
        jr z, _
        cp 0xED
        jr nz, ++_
_:
        inc hl
_:
        inc hl
        ld e, (hl)
        inc hl
        ld d, (hl)
        ex de, hl
        add hl, bc        
        ex de, hl
        ld (hl), d
        dec hl
        ld (hl), e
    
    pop af
    pop bc
    pop de
    pop hl
    ret

; rst 0x20
; Here be dragons
pcall:
    ; Stack state : calling addr
    push af
        ; Stack state : calling addr, original AF
        ld a, i
        ; safety first
        ld a, i
        di
        push af
            ; Stack state : calling addr, original AF, interrupt state
            ; we can't use getBankA because it behaves differently on the 84+CSE, and we don't want that
            in a, (PORT_BANKA)
            push af
                ; Stack state : calling addr, original AF, interrupt state, page
                push hl
                    ; Stack state : calling addr, original AF, interrupt state, page, original HL
                    ld hl, .returnPoint
                    ex (sp), hl
                    push hl
                        ; Stack state : calling addr, original AF, interrupt state, page, return point, original HL
                    pop af
                pop af
            pop af
        pop af
    pop af
    ; SP points on calling addr and tadaaa ! AF holds its original value
    ; too bad that we don't care about it at the moment
    ex (sp), hl
    ; Stack state : original HL, original AF, interrupt state, page, return point, original HL #### HL contains calling addr
#ifdef FLASH4MB
    xor a
    out (PORT_MEMA_HIGH), a
#endif
    ld a, (hl)
    out (PORT_BANKA), a
    inc hl
    ld a, (hl)
    inc hl
    ex (sp), hl
    inc a
    ld l, a
    ; Stack state : calling addr + 2 (ret addr), original AF, interrupt state, page, return point, original HL
    ; SP still points on ret addr
    dec sp \ dec sp
        ; Loads AF with its original value
        pop af \ dec sp \ dec sp
        dec sp \ dec sp
            dec sp \ dec sp
                dec sp \ dec sp
                    dec sp \ dec sp
                    ; SP is on original HL
                        push af
                            ; Stack state : ret addr, original AF, interrupt state, page, return point, original HL, original AF
                            ; HL = 0x8000 - (l + 1) * 3
                            xor a
                            ld h, 0x7f
                            sub l
                            sub l \ jr nc, $ + 3 \ dec h
                            sub l \ jr nc, $ + 3 \ dec h
                            ld l, a
                        pop af
                        ; Stack state : ret addr, original AF, interrupt state, page, return point, original HL
                        ex (sp), hl
                        ; Stack state : ret addr, original AF, interrupt state, page, return point, pcall addr
                        ret
.returnPoint:
    ; Stack state : ret address, original AF, interrupt state, page
                        ; Put AF at (kernel_garbage) for later use
                        ld (kernel_garbage + 2), sp
                        ld sp, kernel_garbage + 2
                        push af
                        ld sp, (kernel_garbage + 2)
#ifdef FLASH4MB
                xor a
                out (PORT_MEMA_HIGH), A
#endif
            pop af
            out (PORT_BANKA), a
        pop af
        jp po, _
    pop af
    ; Fetch AF from backup
    ld (kernel_garbage + 2), sp
    ld sp, kernel_garbage
    pop af
    ld sp, (kernel_garbage + 2)
    ei
    ret
_:  ; Same thing without enabling interrupts
    pop af
    ; Fetch AF from backup
    ld (kernel_garbage + 2), sp
    ld sp, kernel_garbage
    pop af
    ld sp, (kernel_garbage + 2)
    ret

; rst 0x28
bcall:
    push hl
    push af
        ld hl, (bcall_hook)
        xor a
        cp h
        jr nz, _
        cp l
        ; KnightOS doesn't provide bcall support on its own. However, 3rd party programs
        ; can hook into RST $28 and provide their own bcall mechanism. This is to make
        ; compatibility layers possible with KnightOS. However, if no bcall hook is set,
        ; we kill the originating thread. This is because use of a bcall implies that a
        ; TIOS program is running, and without a compatibility layer (especially considering
        ; that it's using bcalls), it's extremely likely to crash the system if allowed
        ; to continue.
        jp z, killCurrentThread
_:  ; We have a hook, call it
    pop af
    jp (hl)
