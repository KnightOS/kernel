;; openFileRead [File Stream]
;;  Opens a file stream in read-only mode.
;; Inputs:
;;  DE: Path to file (string pointer)
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  D: File stream ID (on success)
;;  E: Garbage (on success)
openFileRead:
    push hl
    push bc
        call findFileEntry
        jp nz, .fileNotFound
        ld b, a
        push af
        ld a, i
        push af
        di
        push iy
        push bc
            ld iy, fileHandleTable
            ld bc, 8
            ld d, 0
.findEntryLoop:
            ld a, (iy)
            cp 0xFF
            jr z, .entryFound
            add iy, bc
            inc d
            ld a, maxFileStreams - 1
            cp d
            jr nc, .findEntryLoop
            ; Too many active streams
            ; We could check (activeFileStreams) instead, but since we have to iterate through the table
            ; anyway, we may as well do it here and save some space.
        pop bc
        pop iy
        pop af
    jp po, _
    ei
_:  pop af
    pop bc
    pop hl
    or a
    ld a, errTooManyStreams
    ret
.entryFound:
            ; We can put the stream entry at (iy) and use d as the ID
            pop bc
            ld a, b
            push de
            push ix
                call getCurrentThreadId
                and 0b111111
                ld (iy), a ; Flags & owner
                ; Create a buffer
                ld bc, 256
                call malloc
                jr nz, .outOfMemory
                push ix \ pop bc
                ld (iy + 1), c ; Buffer
                ld (iy + 2), b
                dec hl \ dec hl \ dec hl \ dec hl \ dec hl \ dec hl \ dec hl ; Move HL to middle file size byte
                ; Check for final block
                xor a
                ld (iy + 7), a
                ld a, (hl)
                or a ; cp 0
                jr z, .final
                cp 1
                jr nz, .notFinal
                ; Check next byte to see if it's zero - if so, it's the final block
                inc hl
                ld a, (hl)
                or a ; cp 0
                jr nz, .notFinal - 1 ; (-1 adds the inc hl)
                dec hl
.final:
                set 7, (iy)
                ld a, (hl)
                ld (iy + 7), a
                jr .notFinal
dec hl
.notFinal:
                inc hl
                ld a, (hl)
                ld (iy + 6), a ; Length of final block
                dec hl \ dec hl ; Move to section ID
                ld b, (hl)
                ld (iy + 5), b
                dec hl \ ld c, (hl)
                ld (iy + 4), c
                ; Section ID in BC
                call populateStreamBuffer
                xor a
                ld (iy + 3), a ; Stream pointer
            pop ix
            pop de
        pop iy
        pop af
    jp po, _
    ei
_:  pop af
    pop bc
    pop hl
    ret
.fileNotFound:
    pop bc
    pop hl
    ret
.outOfMemory:
            pop ix
            pop de
        pop iy
        pop af
    jp po, _
    ei
_:  pop af
    pop bc
    pop hl
    ld a, errOutOfMem
    or a
    ret

; Section ID in BC, block in IX
populateStreamBuffer:
    push af
        getBankA
        push af
        push de
        push hl
        push bc
            ld a, c
            or a \ rra \ rra \ rra \ rra \ rra
            and 0b1111
            push bc
                ld c, a
                ld a, b
                rla \ rla \ rla
                and 0b11111000
                or c
            pop bc
            setBankA
            ld a, c
            and 0b11111
            add a, 0x40
            ld h, a
            ld l, 0
            ld bc, 256
            push ix \ pop de
            ldir
        pop bc
        pop hl
        pop de
        pop af
        setBankA
    pop af
    ret

;; getStreamBuffer [File Stream]
;;  Gets the address of a stream's memory buffer.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  IX: Stream buffer (on success)
;; Notes:
;;  For read-only streams, modifying this buffer could have unforseen consequences and it will not be copied back to the file.
;;  For writable streams, make sure you call flushStream if you modify this buffer and want to make the changes persist to the file.
getStreamBuffer:
    push ix
        call getStreamEntry
        jr nz, .fail
        ld l, (ix + 1)
        ld h, (ix + 2)
    pop ix
    ret
.fail:
    pop ix
    ret

;; getStreamEntry [File Stream]
;;  Gets the address of a stream entry in the kernel file stream table.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  IX: File stream entry poitner (on success)
getStreamEntry:
    push af
    push hl
    push bc
        ld a, d
        cp maxFileStreams
        jr nc, .notFound
        or a \ rla \ rla \ rla ; A *= 8
        ld ix, fileHandleTable
        add ixl \ ld ixl, a
        ld a, (ix)
        cp 0xFF
        jr z, .notFound
    pop bc
    inc sp \ inc sp
    pop af
    cp a
    ret
.notFound:
    pop bc
    pop hl
    pop af
    or 1
    ld a, errStreamNotFound
    ret

;; closeStream [File Stream]
;;  Closes an open stream.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
closeStream:
    push ix
        call getStreamEntry
        jr nz, .fail
        push hl
            ld (ix), 0xFF
            ld l, (ix + 1)
            ld h, (ix + 2)
            push hl \ pop ix
            call free
            ld hl, activeFileStreams
            dec (hl)
        pop hl
    pop ix
    cp a
    ret
.fail:
    pop ix
    ret

;; streamReadByte [File Stream]
;;  Reads a single byte from a file stream and advances the stream.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Data read (on success); Error code (on failure)
streamReadByte:
    push ix
        call getStreamEntry
        jr nz, .fail
        push hl
            ld l, (ix + 1)
            ld h, (ix + 2)
            ld a, (ix + 3)
            bit 7, (ix)
            jr z, ++_
            ; check for end of stream
            bit 5, (ix) ; Set on EOF
            jr z, _
        pop hl
    pop ix
    or 1
    ld a, errEndOfStream
    ret
_:          cp (ix + 6)
            jr c, _
            jr nz, _
            ; End of stream!
        pop hl
   pop ix
   or 1
   ld a, errEndOfStream
   ret
_:          add l
            ld l, a
            jr nc, _
            inc h
_:          ld a, (ix + 3)
            add a, 1 ; inc doesn't affect flags
            ld (ix + 3), a
            ld a, (hl)
            jr nc, _
            ; We need to get the next block (or end of stream)
            call getNextBuffer
_:      pop hl
    pop ix
    cp a
    ret
.fail:
    pop ix
    ret

getNextBuffer:
    push af
        bit 7, (ix)
        jr z, _
        ; Set EOF
        set 5, (ix)
    pop af
    ret
_:      push bc
        push hl
        ld a, i
        push af
            di
            call selectSection
            or a \ rlca \ rlca \ inc a \ inc a
            ld l, a
            ld h, 0x40
            ld c, (hl)
            inc hl
            ld b, (hl)
            push ix
                ld l, (ix + 1)
                ld h, (ix + 2)
                push hl \ pop ix
                call populateStreamBuffer
            pop ix
            ; Update the entry in the stream table
            ld (ix + 4), c
            ld (ix + 5), b
            ; Check if this is the last block
            call selectSection
            or a \ rlca \ rlca \ inc a \ inc a
            ld l, a
            ld h, 0x40
            ld a, 0xFF
            cp (hl)
            jr nz, _
            inc hl \ cp (hl)
            jr nz, _
            ; Set last section stuff
            set 7, (ix)
_:      pop af
        jp po, _
        ei
_:      pop hl
        pop bc
    pop af
    ret

; Given stream entry at IX, grabs the section ID, swaps in the page, and sets A to the block index.
; Destroys B
selectSection:
    ld a, (ix + 4)
    rra \ rra \ rra \ rra \ rra \ and 0b111
    ld b, a
    ld a, (ix + 5)
    rla \ rla \ rla \ and 0b11111000
    or b
    setBankA
    ld a, (ix + 4)
    and 0b11111
    ret

;; streamReadWord [File Stream]
;;  Reads a 16-bit word from a file stream and advances the stream.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  HL: Data read (on success)
streamReadWord:
; TODO: Perhaps optimize this into something like streamReadByte
; The problem here is that reading two bytes requires you to do some
; additional bounds checks that would make us basically put the same
; code in twice (i.e. what happens when the word straddles a block
; boundary?  Although the only time this would happen is when the pointer
; is on the last byte of the block.)
    push af
        call streamReadByte
        jr nz, .error
        ld l, a
        call streamReadByte
        jr nz, .error
        ld h, a
    pop af
    ret
.error:
    inc sp \ inc sp
    ret

;; streamReadBuffer [File Stream]
;;  Reads a number of bytes from a file stream and advances the stream.
;; Inputs:
;;  D: Stream ID
;;  IX: Destination address
;;  BC: Length
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;; Notes:
;;  If BC is greater than the remaining space in the stream, the stream will be advanced to the end
;;  before returning an error.
streamReadBuffer:
    push hl \ push bc \ push de \ push af \ push ix ; Chance for optimization - skip all these if not found
        call getStreamEntry
        jr z, .streamFound
    pop ix \ pop de \ pop de \ pop bc \ pop hl
    ret
.streamFound:
        pop de \ push de ; the value of IX before getStreamEntry was called
        ld l, (ix + 1)
        ld h, (ix + 2)
        ld a, (ix + 3)
        add l, a \ ld l, a \ jr nc, $+3 \ inc h
.readLoop:
        ; Registers:
        ; DE: Destination in RAM
        ; HL: Buffer pointer (including current position offset)
        ; IX: File stream entry
        ; BC: Total length left to read

        ; Try to return if BC == 0
        xor a
        cp c
        jr nz, _
        cp b
        jp z, .done

_:      bit 5, (ix) ; Check for EOF
        jr nz, .endOfStream
        push bc
            ; Check if we have enough space left in file stream
            bit 7, (ix) ; Final block?
            jr z, _
            ; Final block.
            cp (ix + 6) ; A is still zero
            jr z, .readOkay
            ld a, (ix + 6)
            cp c
            jr nc, .readOkay
            jr .endOfStream - 1
_:          xor a
            cp c
            jr nz, .readOkay ; 0 < n < 0x100
            ; We need to read 0x100 bytes this round
            bit 7, (ix)
            jr z, .readOkay ; Not the final block, go for it
            cp (ix + 6)
            jr z, .readOkay
            ; Not enough space
        pop bc
.endOfStream:
    pop ix \ pop de \ pop de \ pop bc \ pop hl
    or 1 \ ld a, errEndOfStream \ ret
.readOkay:
            ld b, 0
            ld a, c
            or a ; cp 0
            jr nz, _
            ld bc, 0x100
            ; BC is the amount they want us to read, assuming we're at the start of the block
            ; But we may not be at the start of the block - handle that here
            ; If (amount left in block) is less than BC, set BC to (amount left in block)
            ; See if we can manage a full block
            cp (ix + 3)
            jr z, .doRead
            ; We can't, so truncate
            sub (ix + 3)
            ld c, a
            ld b, 0
            jr .doRead
_:          ; Check for partial blocks (BC != 0x100)
            push de
                push af
                    ; Load the amount we *can* read into B
                    xor a
                    bit 7, (ix)
                    jr z, _
                    ld a, (ix + 6)
_:                  ; Space left in block in A
                    sub (ix + 3)
                    ld d, a
                pop af
                cp d
                jr c, _
                jr z, _
                xor a \ cp d
                jr z, _ ; Handle 0x100 bytes
                ; Too long, truncate a little
                ld c, d
                ld b, 0
_:          pop de
.doRead:
            ; Update HL with stream pointer
            ld l, (ix + 1)
            ld h, (ix + 2)
            ld a, (ix + 3)
            add l, a \ ld l, a
            jr nc, _ \ inc h
_:          ; Do read
            push bc \ ldir \ pop bc
            ; Subtract BC from itself
        pop hl ; Was BC
        or a \ sbc hl, bc
_:      push hl ; Push *new* length to stack so we can remember it while we cycle to the next buffer
            ; Update stream pointer
            ld a, (ix + 3)
            add c
            ld (ix + 3), a

            bit 7, (ix)
            jr z, _
            ; Handle "last buffer"
            cp (ix + 6)
            jr nz, .loopAround
            set 5, (ix)
            jr .loopAround
_:          ; Handle any other buffer
            xor a
            cp (ix + 3)
            jr nz, .loopAround
            ld (ix + 3), a
            call getNextBuffer
            ld l, (ix + 1)
            ld h, (ix + 2)
.loopAround:
            ; Back to the main loop
        pop bc
        jp .readLoop

;.done - 2:
        pop bc
.done:
    pop ix \ pop de \ pop af \ pop bc \ pop hl
    cp a
    ret

;; getStreamInfo [File Stream]
;;  Gets the amount of space remaining in a file stream.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  EBC: Remaining space in stream (on success)
getStreamInfo:
    push ix
        call getStreamEntry
        jr z, .streamFound
    pop ix
    ret
.streamFound:
        bit 5, (ix)
        jr z, _
    pop ix
    ld e, 0
    ld bc, 0
    ret
        ; Start with what remains in the current block
_:      push af \ push af \ push de
            ld e, 0
            ld b, 0
            ; Get size of current block
            bit 7, (ix)
            jr nz, _
            ld a, 0 ; 0x100 bytes
            jr ++_
_:          ld a, (ix + 6)
_:          ; Subtract amount already read from this block
            sub (ix + 3)
            ; And A is now the length left in this block
            ld c, a
            or a ; cp 0
            jr nz, _ \ inc b
_:          bit 7, (ix)
            jr nz, .done ; Leave eary for final block
            ; Loop through remaining blocks
            ld a, i
            push af \ di
                ld l, (ix + 4)
                ld h, (ix + 5)
                ; HL is section ID
                dec b ; Reset B to zero
.loop:
                push hl
                    ld a, l
                    rra \ rra \ rra \ rra \ rra \ and 0b111
                    ld l, a
                    ld a, h
                    rla \ rla \ rla \ and 0b11111000
                    or l
                    setBankA
                pop hl
                ld a, l
                and 0b11111
                rlca \ rlca \ inc a \ inc a
                ld h, 0x40
                ld l, a
                ; (HL) is the section header
                push de
                    ld e, (hl)
                    inc hl
                    ld d, (hl)
                    ex de, hl
                pop de
                ; HL is the next section ID
                ; Check if it's 0xFFFF - if it is, we're done.
                ld a, 0xFF
                cp h
                jr nz, .continue
                cp l
                jr nz, .continue
                ; All done, add the final block length and exit
                ld a, (ix + 6)
                add c
                ld c, a
                jr nc, .done_ei
                ld a, b
                add a, 1
                ld b, a
                jr nc, .done_ei
                inc d
                jr .done_ei
.continue:
                ld a, b ; Update length and continue
                add a, 1
                ld b, a
                jr nc, .loop
                inc d
                jr .loop
.done_ei:
                pop af
            jp po, .done
            ei
.done:
        ; We have to preserve DE through this
        ld a, e
        pop de
        ld e, a
        pop hl \ pop af
    pop ix
    cp a
    ret

;; streamReadToEnd [File Stream]
;;  Reads the remainder of a file stream into memory.
;; Inputs:
;;  D: Stream ID
;;  IX: Destination address
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
streamReadToEnd:
    push bc
    push de
        call getStreamInfo
        jr z, _
    pop de
    pop bc
    ret
_:      pop de \ push de
        call streamReadBuffer
    pop de
    pop bc
    ret
