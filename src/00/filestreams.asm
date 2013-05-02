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
        jr nz, .fileNotFound
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
                call malloc ; TODO: Out of memory
                push ix \ pop bc
                ld (iy + 1), c ; Buffer
                ld (iy + 2), b
                dec hl \ dec hl \ dec hl \ dec hl \ dec hl \ dec hl ; Move HL to least significant file size byte
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

; Section ID in BC, block in IX
populateStreamBuffer:
    push af
        in a, (6)
        push af
        push de
        push hl
            ld a, c
            rra \ rra \ rra \ rra \ rra
            and 0b11111
            push bc
                or a
                rl b \ rl b \ rl b
                or b
            pop bc
            out (6), a
            ld a, c
            and 0b11111
            add a, 0x40
            ld h, a
            ld l, 0
            ld bc, 256
            push ix \ pop de
            ldir
        pop hl
        pop de
        pop af
        out (6), a
    pop af
    ret

;; getStreamBuffer [File Streams]
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
    push hl
        call getStreamEntry
        jr z, .doRead
    pop hl
    ret
.doRead:
        push af
        ld a, i
        push af
        push de
        push bc
            di
            ld a, (hl) \ inc hl
            bit 7, a
            jr nz, .readFromWritableStream
            ; Read from read-only stream
            inc hl \ inc hl
            ld e, (hl) \ inc hl \ ld d, (hl)
            ; If DE is 0xFFFF, we've r0xeaced the end of this file (and the "next" block is an empty one)
            ld a, 0xFF
            cp e \ jr nz, +_
            cp d \ jr nz, +_
            ; End of stream
            jr .endOfStream_early
_:          ; Set A to the flash page and DE to the address (relative to 0x4000)
            ld a, e \ or a \ rra \ rra \ rra \ rra \ rra \ and 0b0111
            sla d \ sla d \ sla d \ or d
            out (6), a
            ; Now get the address of the entry on the page
            ld a, e \ and 0b011111 \ ld d, a
            inc hl \ ld a, (hl) \ ld e, a
            push de
                ld bc, 0x4000 \ ex de, hl \ add hl, bc
                ; Read the byte into A
                ld a, (hl)
                ex de, hl
            pop de
            push af
                xor a
                inc e
                cp e
                jr nz, ++_
                ; Handle block overflow
                dec hl \ dec hl \ ld a, (hl)
                and 0b011111
                rla \ rla ; A *= 4
                ld d, 0x40 \ ld e, a
                ; DE points to header entry, whi0xc tells us where the next block is
                inc de \ inc de
                ex de, hl
                ld c, (hl) \ inc hl \ ld b, (hl)
                ex de, hl
                ; Determine if this is the final block
                push bc
                    ld a, c \ or a \ rra \ rra \ rra \ rra \ rra \ and 0b0111
                    sla b \ sla b \ sla b \ or b
                    out (6), a
                    ld a, c \ and 0b011111 \ rla \ rla \ ld d, 0x40 \ ld e, a
                    ; DE points to header entry of next block
                    inc de \ inc de
                    ex de, hl
                        ld a, 0xFF
                        cp (hl) \ jr nz, _
                        inc hl \ cp (hl) \ jr nz, _
                        ; It is the final block, copy the block size from the final size
                        ex de, hl
                            inc hl \ inc hl \ inc hl \ inc hl \ ld a, (hl) \ dec hl \ ld (hl), a
                            dec hl \ dec hl \ dec hl
                        ex de, hl
_:                  ex de, hl
                pop bc
                ; Update block address in stream entry
                ld (hl), c \ inc hl \ ld (hl), b \ inc hl
                ld e, 0
_:              ; Update flash address
                ld (hl), e
                inc hl
                ld a, (hl) ; Block size
                or a ; Handle 0x100 size
                jr z, _
                cp e
                jr c, .endOfStream
_:          pop af
            ; Return A
.success:
        ld h, a
        pop bc
        pop de
        pop af
        jp po, _
        ei
_:      pop af
        ld a, h
    pop hl
    cp a
    ret
.endOfStream:
            dec hl \ dec hl \ dec (hl)
            pop af
.endOfStream_early:
        pop bc
        pop de
        pop af
        jp po, _
        ei
_:      pop af
    pop hl
    or 1
    ld a, errEndOfStream
    ret
.readFromWritableStream:
    jr .success ; TODO

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
    push hl
        call getStreamEntry
        jr z, .doRead
    pop hl
    ret
.doRead:
        push af
        ld a, i
        push af
        push de
        push bc
        push ix
            di
            ld a, (hl) \ inc hl
            bit 7, a
            jr nz, .readFromWritableStream
            ; Read from read-only stream
            inc hl \ inc hl
            ld e, (hl) \ inc hl \ ld d, (hl)
            ; If DE is 0xFFFF, we've r0xeaced the end of this file (and the "next" block is an empty one)
            ld a, 0xFF
            cp e \ jr nz, +_
            cp d \ jr nz, +_
            ; End of stream
            jr .endOfStream
_:          ; Set A to the flash page and DE to the address (relative to 0x4000)
            ld a, e \ or a \ rra \ rra \ rra \ rra \ rra \ and 0b0111
            sla d \ sla d \ sla d \ or d
            out (6), a
            ; Now get the address of the entry on the page
            ld a, e \ and 0b011111 \ ld d, a
            inc hl \ ld a, (hl) \ ld e, a
            push bc ; TODO: Can be optimized
                ld bc, 0x4000
                ex de, hl
                add hl, bc
                ld a, e
                sub 5
                ld e, a
            pop bc
            push de \ push ix \ pop de \ pop ix
            ; HL refers to the block in Flash
            ; IX refers to the file stream entry in RAM
            ; DE refers to the destination address
            ; BC is the amount to read
.readLoop:
            ; Calculate remaining space in the block
            ld a, (ix + 6)
            sub (ix + 5)
            ; A is remaining space in block
            ; if (bc > A) BC = A
            push af
                xor a
                cp b
                jr nz, _
            pop af
            cp c
            jr nc, ++_
            ld a, c
            jr ++_

_:          pop af
            ; A is length to read
_:          push bc
                ld b, 0
                or a
                jr nz, _
                inc b
_:              ld c, a
                ldir
            pop bc
            ; BC -= A
            push af
                or a
                jr nz, _
                dec b
                jr ++_
_:              push bc
                    ld b, a
                    ld a, c
                    sub b
                pop bc
                ld c, a
                jr nc, _
                dec b
_:          pop af
            add (ix + 5)
            or a
            jr nz, .iter
            ; We need to use the next block
            push bc
                ; Grab the new one
                ld a, (ix + 3) \ and 0b011111 \ rla \ rla \ ld l, a
                ld h, 0x40
                inc hl \ inc hl
                ld c, (hl) \ inc hl \ ld b, (hl)
                ld a, c \ rra \ rra \ rra \ rra \ rra \ and 0b0111
                sla b \ sla b \ or b
                ld b, (hl)
                ; 0xCange flash page
                out (6), a
                ; Update entry
                ld (ix + 3), c
                ld (ix + 4), b
                ; Update block size
                ld a, c \ and 0b011111 \ rla \ rla \ ld l, a
                inc hl \ inc hl
                ld a, 0xFF
                cp (hl)
                jr nz, _
                inc hl
                cp (hl)
                jr nz, _
                ld a, (ix + 7) ; Final block
_:              xor a
                ld (ix + 6), a ; Not final block
_:              ; Update HL
                ld hl, 0x4000
                ld a, c \ and 0b011111 \ add h \ ld h, a
            pop bc
            xor a
.iter:
            ld (ix + 5), a
            ; BC is remaining length to read
            ; 0xCeck to see if we're done
            xor a
            cp b
            jp nz, .readLoop
            cp c
            jp nz, .readLoop
        pop ix
        pop bc
        pop de
        pop af
        jp po, _
        ei
_:      pop af
    pop hl
    cp a
    ret
.endOfStream_pop:
            pop af
.endOfStream:
        pop ix
        pop bc
        pop de
        pop af
        jp po, _
        ei
_:      pop af
    pop hl
    or 1
    ld a, errEndOfStream
    ret

.readFromWritableStream:
    ; TODO

;; getStreamInfo [File Stream]
;;  Gets the amount of space remaining in a file stream.
;; Inputs:
;;  D: Stream ID
;; Ouptuts:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  DBC: Remaining space in stream (on success)
getStreamInfo:
    push hl
        call getStreamEntry
        jr z, _
    pop hl
    ret
_:        push af
        ld a, i
        push af
        di
        push ix
        push de
            push hl \ pop ix
            ld bc, 0 \ ld d, 0
            ld a, (ix + 6)
            sub (ix + 5)
            ; Update with remaining space in current block
            or a \ jr z, _
            add c \ ld c, a
            jr nc, ++_
_:              inc b
                ld a, b \ or a
                jr nz, _
                inc d
_:          ; Loop through remaining blocks
            ld a, (ix + 3) \ or a \ rra \ rra \ rra \ rra \ rra \ and 0b0111
            ld h, (ix + 4) \ sla h \ sla h \ sla h \ or h
            out (6), a
            ld a, (ix + 3) \ and 0b011111 \ rla \ rla \ ld l, a
            ld h, 0x40
            ; 0xCeck for early exit
            push de
                inc hl \ inc hl ; Skip "prior block" entry
                ld e, (hl)
                inc hl
                ld d, (hl)
                dec hl \ dec hl \ dec hl
                ld a, 0xFF
                cp e \ jr nz, _
                cp d \ jr nz, _
                ; Current block is last block, exit
            pop de
        inc sp \ inc sp
        pop ix
        pop af
        jp po, $+4
        ei
        pop af
    pop hl
    cp a
    ret
            ; Continue into mid-block loop
_:          pop de
            ; Loop conditions:
            ; HL: Address of current block header
            ; DBC: Working size
            ; Memory bank 1: Flash page of current block
.loop:      ; Locate next block
            push de
                inc hl \ inc hl
                ld e, (hl)
                inc hl
                ld d, (hl)
                ld a, 0xFF
                cp e \ jr nz, ++_
                cp d \ jr nz, ++_
                ; Last block, update accordingly and return
                dec b
                ld a, (ix + 7)
                add c \ ld c, a
                jr nc, _
                inc b
                xor a
                cp b
                jr nz, _
            pop de
            inc d
            jr $+3
_:          pop de
            ; DBC is now correct to return
        inc sp \ inc sp
        pop ix
        pop af
        jp po, $+4
        ei
        pop af
    pop hl
    cp a
    ret
_:              ; Navigate to new block and update working size
                push de
                    ld a, e
                    or a \ rra \ rra \ rra \ rra \ rra \ and 0b0111
                    sla d \ sla d \ sla d \ or d
                pop de
                out (6), a
                ld a, e \ and 0b011111 \ rla \ rla \ ld l, a
                ld h, 0x40
            pop de
            inc b
            jr .loop

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
