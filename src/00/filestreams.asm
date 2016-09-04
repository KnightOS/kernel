initFilesystem:
    ; Set all file handles to unused
    ld hl, fileHandleTable
    ld (hl), 0xFF
    ld de, fileHandleTable + 1
    ld bc, 8 * maxFileStreams
    ldir
    ret

; If findNode turned up a symlink, this will follow it and
; give the findNode return value for the final destination.
followSymLink:
#define newName kernelGarbage + 2 ; findNode uses kernelGarbage
    push de
        dec hl ; ID
        dec hl \ dec hl ; Entry len
        dec hl \ dec hl ; Parent ID
        ld b, 0
        ld c, (hl) \ dec hl ; Name length
        scf \ ccf
        sbc hl, bc

.recurse:
        ld de, newName
.load_target_loop:
        ld a, (hl)
        ld (de), a
        inc de \ dec hl
        ; NOTE: KFS spec doesn't mention the trailing zero, this should be standardized
        or a
        jr nz, .load_target_loop
        
        ld de, newName
        call findNode
        jr nz, .not_found

        setBankA
        ld a, (hl)

        cp fsSymLink
        jr z, .recurse

        cp a
.not_found:
    pop de
    ret
#undefine newName

;; openFileRead [Filestreams]
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
        push af
        ld a, i
        push af
        di
        push iy
        push bc
            push de
.linkLoop:
                call findNode
                jp nz, .fileNotFound

                ; Check if node is file or symlink or other
                setBankA
                ld a, (hl)
                cp fsFile
                jr z, .validNode

                cp fsSymLink
                jp nz, .fileNotFound ; TODO: use something like errNotAFile?

                call followSymLink
                jp nz, .fileNotFound
.validNode:
            pop de
            ld iy, fileHandleTable
            ld bc, FILE_HANDLE_SIZE
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
            push de
            push ix
            push hl
            push af
                call getCurrentThreadId
                and 0b111111
                ld (iy + FILE_FLAGS), a ; Flags & owner
                ; Create a buffer
                ld bc, KFS_BLOCK_SIZE
                call malloc
                jp nz, .outOfMemory
                push ix \ pop bc
                ld (iy + FILE_BUFFER), c ; Buffer
                ld (iy + FILE_BUFFER + 1), b
                dec hl \ dec hl \ dec hl \ dec hl \ dec hl \ dec hl \ dec hl ; Move HL to middle file size byte
                ; Check for final block
                xor a
                ld (iy + FILE_FINAL_LENGTH), a
                ld a, (hl)
                or a ; cp 0
                jr z, .final
                cp 1
                jr nz, .notFinal
                ; Check next byte to see if it's zero - if so, it's the final block
                inc hl
                ld a, (hl)
                or a ; cp 0
                jr nz, .notFinal - 1 ; (-1 adds the dec hl)
                dec hl
.final:
                set 7, (iy)
                ld a, (hl)
                ld (iy + FILE_FINAL_LENGTH), a
                jr .notFinal
                dec hl
.notFinal:
                inc hl
                ld a, (hl)
                ld (iy + FILE_FINAL_LENGTH), a ; Length of final block
                dec hl \ dec hl \ dec hl ; Move to section ID
                ld c, (hl)
                ld (iy + FILE_SECTION_ID), c
                dec hl \ ld b, (hl)
                ld (iy + FILE_SECTION_ID + 1), b
                ; Section ID in BC
                call populateStreamBuffer
                ; Populate the previous section, which is 0xFFFF for new streams
                ld a, 0xFF
                ld (iy + FILE_PREV_SECTION), a
                ld (iy + FILE_PREV_SECTION + 1), a
                xor a
                ld (iy + FILE_STREAM), a ; Stream pointer
            pop af
            pop hl
            pop ix
            pop de
            ; Load file entry info
            ld (iy + FILE_ENTRY_PAGE), a
            ld (iy + FILE_ENTRY_PTR), l
            ld (iy + FILE_ENTRY_PTR + 1), h
            ; And the working file size
            ; This doesn't matter for ro streams
            xor a
            ld (iy + FILE_WORKING_SIZE), a
            ld (iy + FILE_WORKING_SIZE + 1), a
            ld (iy + FILE_WORKING_SIZE + 2), a
        pop iy
        pop af
    jp po, _
    ei
_:  pop af
    pop bc
    pop hl
    cp a
    ret
.fileNotFound:
            pop de
        pop bc
        pop iy
        pop af
        jp po, _
        ei
_:      pop af
    pop bc
    pop hl
    ld a, errFileNotFound
    cp 0
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

; TODO: Error out if the parent directory does not exist
; Right now, closeStream is the first time that's ever checked
;; openFileWrite [Filestreams]
;;  Opens a file stream in write mode. If the file does not exist,
;;  it is created.
;; Inputs:
;;  DE: Path to file (string pointer)
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  D: File stream ID (on success)
;;  E: Garbage (on success)
openFileWrite:
    push hl
    push bc
    push af
    ld a, i
    push af
    di
        push iy
        push bc
            ld iy, fileHandleTable
            ld bc, FILE_HANDLE_SIZE
            ld l, 0
.findEntryLoop:
            ld a, (iy + FILE_FLAGS)
            cp 0xFF
            jr z, .entryFound
            add iy, bc
            inc l
            ld a, maxFileStreams - 1
            cp l
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
            push hl
                call findNode ; TODO: Check if this is a file, follow symlinks, etc
                jp nz, .fileNotFound
            pop de
            ld d, e
            ; The rest of this code path *only* covers existing files that are being overwritten
            ; The case of brand-new files is handled in .fileNotFound
            ; We can put the stream entry at (iy) and use d as the ID
            push de
            push ix
            push hl
            push af
                call getCurrentThreadId
                and 0b111111
                or 0b01000000 ; Set writable
                ld (iy), a ; Flags & owner
                ; Create a buffer
                ld bc, KFS_BLOCK_SIZE
                ld a, 1
                call calloc
                jp nz, .outOfMemory
                push ix \ pop bc
                ld (iy + FILE_BUFFER), c ; Buffer
                ld (iy + FILE_BUFFER + 1), b
                dec hl \ dec hl \ dec hl \ dec hl \ dec hl \ dec hl \ dec hl ; Move HL to middle file size byte
                ; Check for final block
                ; Note to self: how on earth does this properly check if this is the last block
                ; Will this fail on files >64K in length?
                ; Someone should examine this more closely later, I'm in the middle of something.
                ; openFileRead is likely affected by the same.
                xor a
                ld (iy + FILE_FINAL_LENGTH), a
                ld a, (hl)
                or a ; cp 0
                jr z, .final
                cp 1
                jr nz, .notFinal
                ; Check next byte to see if it's zero - if so, it's the final block
                inc hl
                ld a, (hl)
                or a ; cp 0
                jr nz, .notFinal - 1 ; -1 adds dec hl
                dec hl
.final:
                set 7, (iy + FILE_FLAGS)
                ld a, (hl)
                ld (iy + FILE_FINAL_LENGTH), a
                jr .notFinal
                dec hl
.notFinal:
                inc hl
                ld a, (hl)
                ld (iy + FILE_FINAL_LENGTH), a ; Length of final block
                dec hl \ dec hl \ dec hl ; Move to section ID
                ld c, (hl)
                ld (iy + FILE_SECTION_ID), c
                dec hl \ ld b, (hl)
                ld (iy + FILE_SECTION_ID + 1), b
                ; Section ID in BC
                call populateStreamBuffer
                ; Populate the previous section, which is 0xFFFF for new streams
                ld a, 0xFF
                ld (iy + FILE_PREV_SECTION), a
                ld (iy + FILE_PREV_SECTION + 1), a
                ; Stream pointer
                xor a
                ld (iy + FILE_STREAM), a
            pop af
            pop hl
            pop ix
            pop de
            push af
                 ; Load file entry info
                 ld (iy + FILE_ENTRY_PAGE), a
                 ld (iy + FILE_ENTRY_PTR), l
                 ld (iy + FILE_ENTRY_PTR + 1), h
                 ld a, 1 ; Stream is flushed
                 ld (iy + FILE_WRITE_FLAGS), a ; Set stream write flags (TODO: Move flags around)
            pop af
            setBankA
            ld bc, 6
            or a
            sbc hl, bc
            ld a, (hl)
            ld (iy + FILE_WORKING_SIZE), a
            dec hl \ ld a, (hl)
            ld (iy + FILE_WORKING_SIZE + 1), a
            dec hl \ ld a, (hl)
            ld (iy + FILE_WORKING_SIZE + 2), a
.done:
_:      pop bc
        pop iy
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    pop hl
    ret
.fileNotFound:
    ;push hl
    push af
        ; Set up some important parts of the file entry first
        ex de, hl
        call strlen
        inc bc
        push ix
             call malloc
             ; TODO: Handle out of memory
             push ix \ pop de
        pop ix
        push de
             ldir
        pop de

        ; FILE_ENTRY_PAGE and FILE_ENTRY_PTR are not important when working with brand-new files
        ; We use them instead to save the file name to memory so we can use it later.
        ; FILE_ENTRY_PAGE set to zero indicates this special case.
        xor a
        ld (iy + FILE_ENTRY_PAGE), a
        ld (iy + FILE_ENTRY_PTR), e
        ld (iy + FILE_ENTRY_PTR + 1), d
    pop af
    ; Now we just have to populate the rest of this file handle
    push ix
        call getCurrentThreadId
        and 0b111111
        or 0b01000000 ; Set writable
        ld (iy + FILE_FLAGS), a ; Flags & owner
        ; Create a buffer
        ld bc, KFS_BLOCK_SIZE
        ld a, 1
        call calloc
        jp nz, .outOfMemory
        push ix \ pop bc
        ld (iy + FILE_BUFFER), c ; Buffer
        ld (iy + FILE_BUFFER + 1), b

        xor a
        ld (iy + FILE_FINAL_LENGTH), a
        set 7, (iy + FILE_FLAGS) ; Set "final block"

        ld a, 0xFF
        ld (iy + FILE_SECTION_ID), a
        ld (iy + FILE_SECTION_ID + 1), a
        ld (iy + FILE_PREV_SECTION), a
        ld (iy + FILE_PREV_SECTION + 1), a

        xor a
        ld (iy + FILE_STREAM), a

        ld (iy + FILE_WORKING_SIZE), a
        ld (iy + FILE_WORKING_SIZE + 1), a
        ld (iy + FILE_WORKING_SIZE + 2), a

        inc a ; Stream is flushed
        ld (iy + FILE_WRITE_FLAGS), a ; TODO: Move flags around
        ; End of stream
        set 5, (iy + FILE_FLAGS)
    pop ix
    pop hl
    ld d, l
    jp .done
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
            ld hl, 0xFFFF
            call cpHLBC
            jr z, _
            ld a, b
            setBankA
            ld a, c
            add a, 0x40
            ld h, a
            ld l, 0
            ld bc, KFS_BLOCK_SIZE
            push ix \ pop de
            ldir
        pop bc
        pop hl
        pop de
        pop af
        setBankA
    pop af
    ret
_:          push ix \ pop de ; New blocks get to be 0xFF
            ld h, d \ ld l, e
            inc de
            ld a, 0xFF
            ld (hl), a
            ld bc, KFS_BLOCK_SIZE
            ldir
        pop bc
        pop hl
        pop de
        pop af
        setBankA
    pop af
    ret

;; getStreamBuffer [Filestreams]
;;  Gets the address of a stream's memory buffer.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  HL: Stream buffer (on success)
;; Notes:
;;  For read-only streams, modifying this buffer could have unforseen consequences and it will not be copied back to the file.
;;  For writable streams, make sure you call [[flush]] if you modify this buffer and want to make the changes persist to the file.
getStreamBuffer:
    push ix
        call getStreamEntry
        jr nz, .fail
        ld l, (ix + FILE_BUFFER)
        ld h, (ix + FILE_BUFFER + 1)
    pop ix
    ret
.fail:
    pop ix
    ret

; getStreamEntry [Filestreams]
;  Gets the address of a stream entry in the kernel file stream table.
;  Note that it is almost always better to use the kernel functions for
;  getting data out of this, because the internal struct layout may
;  change between kernel releases.
; Inputs:
;  D: Stream ID
; Outputs:
;  Z: Set on success, reset on failure
;  A: Error code (on failure)
;  IX: File stream entry pointer (on success)
getStreamEntry:
    push af
    push hl
    push bc
        ld a, d
        cp maxFileStreams
        jr nc, .notFound
        or a \ rla \ rla \ rla \ rla ; A *= 16 (length of file handle)
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

;; closeStream [Filestreams]
;;  Closes an open stream.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
closeStream:
    push ix
    push af
    ld a, i
    push af
    di
        call getStreamEntry
        jr nz, .fail
        bit 6, (ix)
        jr nz, .closeWritableStream
        push hl
            ld (ix + FILE_FLAGS), 0xFF
            ld l, (ix + FILE_BUFFER)
            ld h, (ix + FILE_BUFFER + 1)
            push hl \ pop ix
            call free
            ld hl, activeFileStreams
            dec (hl)
        pop hl
    pop af
    jp po, _
    ei
_:  pop af
    pop ix
    cp a
    ret
.fail:
    pop ix
    ret
.closeWritableStream:
        push bc
        push de
        push hl
        push iy
            call flush_withStream

            xor a
            ld l, (ix + FILE_ENTRY_PTR)
            ld h, (ix + FILE_ENTRY_PTR + 1)
            cp (ix + FILE_ENTRY_PAGE)
            jp nz, .overwriteFile
            ; Find the parent directory and extract the file name alone
            call strlen
            inc bc
            ld de, kernelGarbage + 0x100
            push bc
                 ldir
            pop bc
            ld hl, kernelGarbage + 0x100
            add hl, bc
            ld a, '/'
            cpdr
            inc hl
            xor a
            ld (hl), a
            push hl
                ld de, kernelGarbage + 0x100
                call findNode ; TODO: Check that this is a directory node
                jp nz, .wtf
                setBankA
            pop de
            ex de, hl
            ld a, '/'
            ld (hl), a
            inc hl
            push hl
                ex de, hl
                dec hl \ dec hl \ dec hl
                dec hl \ dec hl
                ld e, (hl) \ dec hl \ ld d, (hl) ; Parent dir

                ld a, (ix + FILE_WORKING_SIZE + 2)
                ld c, (ix + FILE_WORKING_SIZE)
                ld b, (ix + FILE_WORKING_SIZE + 1)
                ld l, (ix + FILE_SECTION_ID)
                ld h, (ix + FILE_SECTION_ID + 1)
                ; Traverse the sections to find the first
                ; TODO: We can skip one iteration by pulling the PREV_SECTION_ID
                ; from the file handle
                push af
                push de
                push hl
_:                  ld a, h
                    setBankA
                    ld a, l
                    rlca \ rlca
                    ld l, a
                    ld h, 0x40
                    ld e, (hl)
                    inc hl
                    ld d, (hl)
                    ex de, hl
                    ld de, 0x7FFF
                    call cpHLDE
                    jr z, _
                    inc sp \ inc sp \ push hl
                jr -_
_:              pop hl
                pop de
                pop af
                push hl \ pop iy
            pop hl
.resumeWrite:
            call createFileEntry
            jp nz, .wtf + 2

            ; Clear away file handle
            push hl
                ld (ix), 0xFF
                ld l, (ix + FILE_BUFFER)
                ld h, (ix + FILE_BUFFER + 1)
                push hl \ pop ix
                call free
                ld hl, activeFileStreams
                dec (hl)
            pop hl
        pop iy
        pop hl
        pop de
        pop bc
    pop af
    jp po, _
    ei
_:  pop af
    pop ix
    cp a
    ret
.overwriteFile:
            ld a, (ix + FILE_ENTRY_PAGE)
            ld l, (ix + FILE_ENTRY_PTR)
            ld h, (ix + FILE_ENTRY_PTR + 1)
            setBankA
            ld a, fsModifiedFile
            call unlockFlash
            call writeFlashByte ; Mark old entry as modified
            call lockFlash
            ; Load appropriate values for createFileEntry
            dec hl \ dec hl \ dec hl
            ld e, (hl)
            dec hl
            ld d, (hl) ; Parent ID
            push de
                ; Grab file name, too
                ld bc, -7
                add hl, bc

                ld bc, 0
                ld de, kernelGarbage + 0x100
_:              ld a, (hl)
                ld (de), a
                dec hl
                inc de
                inc bc
                or a
                jr nz, -_

                ld l, (ix + FILE_SECTION_ID)
                ld h, (ix + FILE_SECTION_ID + 1)
                ; Traverse to find first section
                push af
                push hl
_:                  ld a, h
                    setBankA
                    ld a, l
                    rlca \ rlca
                    and 0b11111100
                    ld l, a
                    ld h, 0x40
                    ld e, (hl)
                    inc hl
                    ld d, (hl)
                    ex de, hl
                    ld de, 0x7FFF
                    call cpHLDE
                    jr z, _
                    inc sp \ inc sp \ push hl
                jr -_
_:              pop hl
                pop af
                push hl \ pop iy

                ex de, hl
                ld hl, kernelGarbage + 0x100 ; File name

                ld a, (ix + FILE_WORKING_SIZE + 2)
                ld c, (ix + FILE_WORKING_SIZE)
                ld b, (ix + FILE_WORKING_SIZE + 1)
            pop de
            jp .resumeWrite
.wtf:
            pop hl
        pop iy
        pop hl
        pop de
        pop bc
    pop af
    jp po, _
    ei
_:  pop af
    pop ix
    ld a, 'W'|'T'|'F'
    or 1
    ret

; Flushes writes and loads the next block
advanceBlock:
    call flush
    ret nz
    push ix
    push hl
    push bc
    push af
    ld a, i
    push af
    di
        call getStreamEntry
        ld l, (ix + FILE_SECTION_ID)
        ld h, (ix + FILE_SECTION_ID + 1)
        ld (ix + FILE_PREV_SECTION), l
        ld (ix + FILE_PREV_SECTION + 1), h
        ; Grab the next block
        ld a, h
        setBankA
        ld a, l
        rlca \ rlca \ inc a \ inc a
        ld l, a
        ld h, 0x40
        ld c, (hl)
        inc hl
        ld b, (hl)
        ld (ix + FILE_SECTION_ID), c
        ld (ix + FILE_SECTION_ID + 1), b
        ld l, (ix + FILE_BUFFER)
        ld h, (ix + FILE_BUFFER + 1)
        push hl \ pop ix
        call populateStreamBuffer
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    pop hl
    pop ix
    ret

;; flush [Filestreams]
;;  Flushes pending writes to disk.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  A: Preserved on success; error code on error
;;  Z: Set on success, reset on error
;; Notes:
;;  This happens periodically as you write to the stream, and happens
;;  automatically on closeStream. Try not to use it unless you have to.
flush:
    push ix
    push af
    ld a, i
    push af
    di
        call getStreamEntry
        jp nz, _flush_fail
        bit 6, (ix + FILE_FLAGS)
        jp z, _flush_exitEarly ; Do nothing if not writable
_flush_withStream:
        bit 0, (ix + FILE_WRITE_FLAGS) ; Check if flushed
        jp nz, _flush_exitEarly
        push ix
        push hl
        push bc
        push de
            ; Find a free block
            ld a, 4
.pageLoop:
            setBankA
            ld hl, 0x4000 + 5
.searchLoop:
            ld a, (hl)
            bit 7, a
            jr nz, .freeBlockFound
            inc hl \ inc hl \ inc hl \ inc hl
            ld bc, 0x4101 ; End of section
            call cpHLBC
            jr nz, .searchLoop
            ; Next page
            getBankA
            inc a
            ; TODO: Stop at end of filesystem
            jr .pageLoop
.freeBlockFound:
            dec hl
            push hl
                ; Convert HL into section ID
                sra l \ sra l ; L /= 4 to get index
                ld a, l
                and 0b00111111
                ld l, a
                getBankA
                ld h, a
                push hl
                    ; Write buffer to disk
                    ld a, l
                    add a, 0x40
                    call getStreamBuffer ; At this point, D is still the stream ID
                    ld d, a
                    ld e, 0
                    ld bc, KFS_BLOCK_SIZE
                    call unlockFlash
                    call writeFlashBuffer
                pop hl
            pop de
            ; HL is new section ID, DE is header pointer
.firstSection:
            push hl
                ; We have to check to see if the section we're editing is already allocated, and if
                ; so, we have to reallocate it elsewhere.
                ; Best case - current section ID is set to 0xFFFF
                ld c, (ix + FILE_PREV_SECTION)
                ld b, (ix + FILE_PREV_SECTION + 1)
                res 7, b
                ld (kernelGarbage), bc
                ld bc, 0xFFFF
                ld l, (ix + FILE_SECTION_ID)
                ld h, (ix + FILE_SECTION_ID + 1)
                call cpHLBC ; BC == 0xFFFF
                jr z, _ ; Current section ID is 0xFFFF, so skip this
                ; Grab the next section ID from the obsolete section
                getBankA
                push af
                    ld a, h
                    setBankA
                    ld a, l
                    rlca \ rlca \ and 0b11111100 \ inc a \ inc a
                    ld l, a
                    ld h, 0x40
                    ld c, (hl)
                    inc hl
                    ld b, (hl)
                pop af
                setBankA
_:              ld (kernelGarbage + 2), bc
                ld bc, 4
                ld hl, kernelGarbage
                call writeFlashBuffer
                ld bc, (kernelGarbage + 2) ; Grab next section ID again
                ld hl, 0xFFFF
                call cpHLBC
            pop hl \ push hl
                call nz, flush_updateNextSection
                ; Update previous section's header if needed
                ld l, (ix + FILE_PREV_SECTION)
                ld h, (ix + FILE_PREV_SECTION + 1)
                ld bc, 0xFFFF
                call cpHLBC
                jr z, _ ; "if needed"
                ; HL is previous section ID (needs updating)
                ld c, l \ ld b, h
            pop hl \ push hl
                call flush_updatePreviousSection
_:          pop hl
            ; End wrong
            ; Load current section ID into file handle
            ld (ix + FILE_SECTION_ID), l
            ld (ix + FILE_SECTION_ID + 1), h
.done:
            call lockFlash
        pop de
        pop bc
        pop hl
        pop ix
        set 0, (ix + FILE_WRITE_FLAGS) ; Mark as flushed
_flush_exitEarly:
    pop af
    jp po, _
    ei
_:  pop af
    pop ix
    cp a
    ret
_flush_fail:
        call lockFlash
    pop af
    jp po, _
    ei
_:  pop af
    pop ix
    or 1
    ld a, errStreamNotFound
    ret
flush_withStream:
    push ix
    push af
    ld a, i
    push af
        jp _flush_withStream

flush_updatePreviousSection:
    push de ; Can destroy all others
        ld a, b
        setBankA
        ld a, c
        rlca \ rlca \ inc a \ inc a
        ld e, a
        ld d, 0x40
        ; We'll do a quick check to make sure we really really have to erase flash
        ; Perform A & B ^ B for both octets and if Z is set, we don't need to erase
        ex de, hl
        ld a, (hl)
        and e
        xor e
        jr nz, .mustErase + 1
        inc hl
        ld a, (hl)
        and d
        xor d
        jr nz, .mustErase
        ; Woo we can do it without an erasure
        ex de, hl
        ld (kernelGarbage), hl
        dec de
        ld bc, 2
        ld hl, kernelGarbage
        call writeFlashBuffer
    pop de
    ret
.mustErase:
        dec hl
        ex de, hl
        jr flush_update_mustErase
; We have to do a flash erasure for this procedure
; HL is the new section ID
; BC is the section to update
; This is the worst case for performance, to be avoided
; The performance of this can probably be made better by way of some custom RAM shellcode
; instead of using the flash driver directly
; To be investigated later, this is an edge case after all
flush_updateNextSection:
    push de ; Can destroy all others
        ld a, b
        setBankA
        ld a, c
        rlca \ rlca
        ld e, a
        ld d, 0x40
        ; We'll do a quick check to make sure we really really have to erase flash
        ; Perform A & B ^ B for both octets and if Z is set, we don't need to erase
        ex de, hl
        ld a, (hl)
        and e
        xor e
        jr nz, .mustErase + 1
        inc hl
        ld a, (hl)
        and d
        xor d
        jr nz, .mustErase
        ; Woo we can do it without an erasure
        ex de, hl
        ld (kernelGarbage), hl
        dec de
        ld bc, 2
        ld hl, kernelGarbage
        call writeFlashBuffer
    pop de
    ret
.mustErase:
        dec hl
        ex de, hl
flush_update_mustErase:
        ; DE points to the address that needs fixing up
        ; HL is the value to fix it up to
        getBankA
        push de
        push hl
            ; Clear the sector and restore all but the affected page
            call copySectorToSwap
            call eraseFlashSector

            ld b, swapSector
            ld d, a ; Flash page being dealt with
            and 0b11111100 ; Start of sector
            ld e, a
_:          or e
            cp d
            call nz, copyFlashPage
            inc a
            inc b
            and 0b00000011
            jr nz, -_
.modifiedPage:
            ; Re-write the old page, minus the first 0x200 bytes
            ld a, d
            and 0b00000011
            add a, swapSector
            ld b, a
            ld a, d
            ; A is source page
            ; B is destination page
            ld hl, 0x4042 ; Skip the first 0x200 bytes
            call copyFlashExcept
            ld a, b
            setBankA
            ld a, d
            ld hl, 0x4000
            ld de, kernelGarbage
            ld bc, 0x200
            ldir ; Move the header into kernelGarbage to edit
            setBankA
        pop hl
        pop de
        ex de, hl
        ld h, kernelGarbage >> 8 ; TODO: What if kernelGarbage isn't aligned?
        ld (hl), e
        inc hl
        ld (hl), d
        ld hl, kernelGarbage
        ld de, 0x4000
        ld bc, 0x200
        call writeFlashBuffer
    pop de
    ret

;; streamWriteByte [Filestreams]
;;  Writes a single byte to a file stream and advances the stream.
;; Inputs:
;;  D: Stream ID
;;  A: Value
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
streamWriteByte:
    push ix
    push af
        call getStreamEntry
        jr z, .writeByte
.errStreamNotFound:
    pop af
    pop ix
    or 1
    ld a, errStreamNotFound
    ret
.errNotWritable:
    pop af
    pop ix
    or 1
    ld a, errReadOnly
    ret
.writeByte:
        bit 6, (ix + FILE_FLAGS)
        jr z, .errNotWritable
        push hl
        push bc
            ld l, (ix + FILE_BUFFER)
            ld h, (ix + FILE_BUFFER + 1)
            ld b, 0
            ld c, (ix + FILE_STREAM)
            add hl, bc
            ld (hl), a ; Write actual value
            ; Mark stream as not flushed
            res 0, (ix + FILE_WRITE_FLAGS)
            ; Advance stream
            inc c
            ld (ix + FILE_STREAM), c
            xor a
            cp c
            call z, advanceBlock
            ; Bump file size if at end of stream
            bit 5, (ix + FILE_FLAGS)
            jr z, _
            ld c, (ix + FILE_WORKING_SIZE)
            ld b, (ix + FILE_WORKING_SIZE + 1)
            inc bc
            ld a, c
            or b
            jr nz, $+5 \ inc (ix + FILE_WORKING_SIZE + 2)
            ld (ix + FILE_WORKING_SIZE), c
            ld (ix + FILE_WORKING_SIZE + 1), b
_:      pop bc
        pop hl
    pop af
    pop ix
    cp a
    ret

;; streamWriteWord [Filestreams]
;;  Writes a 16-bit word to a file stream and advances the stream.
;; Inputs:
;;  D: Stream ID
;;  HL: Value
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
streamWriteWord:
    push af
        ld a, l
        call streamWriteByte
        jr nz, .error
        ld a, h
        call streamWriteByte
        jr nz, .error
    pop af
    ret
.error:
    inc sp \ inc sp
    ret

;; streamReadByte [Filestreams]
;;  Reads a single byte from a file stream and advances the stream.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Data read (on success); Error code (on failure)
streamReadByte:
    push ix
        push hl
            call getStreamEntry
            jr nz, .fail
            ld l, (ix + FILE_BUFFER)
            ld h, (ix + FILE_BUFFER + 1)
            ld a, (ix + FILE_STREAM)
            bit 7, (ix + FILE_FLAGS)
            jr z, ++_
            ; check for end of stream
            bit 5, (ix + FILE_FLAGS) ; Set on EOF
            jr z, _
        pop hl
    pop ix
    or 1
    ld a, errEndOfStream
    ret
_:          cp (ix + FILE_FINAL_LENGTH)
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
_:          ld a, (ix + FILE_STREAM)
            add a, 1 ; inc doesn't affect flags
            ld (ix + FILE_STREAM), a
            ld a, (hl)
            jr nc, _
            ; We need to get the next block (or end of stream)
            call getNextBuffer
_:      pop hl
    pop ix
    cp a
    ret
.fail:
    pop hl
    pop ix
    ret

getNextBuffer:
    push af
        bit 7, (ix + FILE_FLAGS)
        jr z, _
        ; Set EOF
        set 5, (ix + FILE_FLAGS)
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
                ld l, (ix + FILE_BUFFER)
                ld h, (ix + FILE_BUFFER + 1)
                push hl \ pop ix
                call populateStreamBuffer
            pop ix
            ; Update the entry in the stream table
            ld (ix + FILE_SECTION_ID), c
            ld (ix + FILE_SECTION_ID + 1), b
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
            set 7, (ix + FILE_FLAGS)
_:      pop af
        jp po, _
        ei
_:      pop hl
        pop bc
    pop af
    ret

; Given stream entry at IX, grabs the section ID, swaps in the page, and sets A to the block index.
selectSection:
    ld a, (ix + FILE_SECTION_ID + 1)
    setBankA
    ld a, (ix + FILE_SECTION_ID)
    ret

;; streamReadWord [Filestreams]
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
; boundary?    Although the only time this would happen is when the pointer
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

;; streamWriteBuffer [Filestreams]
;;  Writes a buffer of bytes to a file stream and advances the stream.
;; Inputs:
;;  D: Stream ID
;;  IX: Buffer address
;;  BC: Length
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
streamWriteBuffer:
    push ix
    push de
    push hl
    push af
        push ix \ pop hl
        call getStreamEntry
        jr z, .continue
.errStreamNotFound:
    pop af
    pop hl
    pop de
    pop ix
    or 1
    ld a, errStreamNotFound
    ret
.errNotWritable:
    pop af
    pop hl
    pop de
    pop ix
    or 1
    ld a, errReadOnly
    ret
.continue:
        bit 6, (ix + FILE_FLAGS)
        jr z, .errNotWritable
.loop:
        xor a
        cp b \ jr nz, .write
        cp c \ jr z, .done
.write:
        ; Note: 256 is represented with 0 in 8-bit registers here
        ld a, (ix + FILE_STREAM)
        neg ; Amount left in the buffer
        or a
        jr z, _
        cp c
        jr c, ++_
_:      ld a, c
        or a ; Handle A == 0 (i.e. 256)
        jr nz, _
        ld a, (ix + FILE_STREAM)
        neg
_:      ; A is the number of bytes to write this time around the loop
        push bc
        push af
            push de
                ld e, (ix + FILE_BUFFER)
                ld d, (ix + FILE_BUFFER + 1)
                ld b, 0
                ld c, (ix + FILE_STREAM)
                ex de, hl
                add hl, bc
                ex de, hl
                ld c, a
                or a \ jr nz, $+3 \ inc b ; 0 == 256
                push bc
                    ldir
                pop bc
                res 0, (ix + FILE_WRITE_FLAGS) ; Mark as not flushed
                ld a, (ix + FILE_STREAM)
                add a, c
                ld (ix + FILE_STREAM), a
            pop de
            bit 7, (ix + FILE_FLAGS)
            call z, .check_end
            or a ; cp 0
            call z, advanceBlock
            bit 5, (ix + FILE_FLAGS) ; eof
            jr z, .done_2
            ld a, (ix + FILE_WORKING_SIZE)
            add c
            ld (ix + FILE_WORKING_SIZE), a
            jr nc, _ \ inc (ix + FILE_WORKING_SIZE + 1)
_:          jr nc, _ \ inc (ix + FILE_WORKING_SIZE + 2)
_:          ld a, (ix + FILE_WORKING_SIZE + 1)
            add b
            ld (ix + FILE_WORKING_SIZE + 1), a
            jr nc, _ \ inc (ix + FILE_WORKING_SIZE + 2)
_:      pop af
        pop bc

        neg
        add a, c
        ld c, a
        jr c, .loop
        dec b
        jr .loop
.done:
    pop af
    pop hl
    pop de
    pop ix
    cp a
    ret
.done_2:
        pop af
        pop bc
    jr .done
.check_end:
        ; Checks if the amount written increases the file size
        push af
        push bc
            ld a, (ix + FILE_STREAM)
            ld c, a
            ld a, (ix + FILE_FINAL_LENGTH)
            cp c
            jr nc, .nope
            set 5, (ix + FILE_STREAM) ; set eof
.nope:
        pop bc
        pop af
        ret

;; streamReadBuffer [Filestreams]
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
        ld l, (ix + FILE_BUFFER)
        ld h, (ix + FILE_BUFFER + 1)
        ld a, (ix + FILE_STREAM)
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

_:      bit 5, (ix + FILE_FLAGS) ; Check for EOF
        jr nz, .endOfStream
        push bc
            ; Check if we have enough space left in file stream
            bit 7, (ix + FILE_FLAGS) ; Final block?
            jr z, _
            ; Final block.
            cp (ix + FILE_FINAL_LENGTH) ; A is still zero
            jr z, .readOkay
            ld a, (ix + FILE_FINAL_LENGTH)
            cp c
            jr nc, .readOkay
            jr .endOfStream - 1
_:          xor a
            cp c
            jr nz, .readOkay ; 0 < n < 0x100
            ; We need to read 0x100 bytes this round
            bit 7, (ix + FILE_FLAGS)
            jr z, .readOkay ; Not the final block, go for it
            cp (ix + FILE_FINAL_LENGTH)
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
            ld bc, KFS_BLOCK_SIZE
            ; BC is the amount they want us to read, assuming we're at the start of the block
            ; But we may not be at the start of the block - handle that here
            ; If (amount left in block) is less than BC, set BC to (amount left in block)
            ; See if we can manage a full block
            cp (ix + FILE_STREAM)
            jr z, .doRead
            ; We can't, so truncate
            sub (ix + FILE_STREAM)
            ld c, a
            ld b, 0
            jr .doRead
_:          ; Check for partial blocks (BC != 0x100)
            push de
                push af
                    ; Load the amount we *can* read into B
                    xor a
                    bit 7, (ix + FILE_FLAGS)
                    jr z, _
                    ld a, (ix + FILE_FINAL_LENGTH)
_:                  ; Space left in block in A
                    sub (ix + FILE_STREAM)
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
_:              pop de
.doRead:
            ; Update HL with stream pointer
            ld l, (ix + FILE_BUFFER)
            ld h, (ix + FILE_BUFFER + 1)
            ld a, (ix + FILE_STREAM)
            add l, a \ ld l, a
            jr nc, _ \ inc h
_:          ; Do read
            push bc \ ldir \ pop bc
            ; Subtract BC from itself
        pop hl ; Was BC
        or a \ sbc hl, bc
_:          push hl ; Push *new* length to stack so we can remember it while we cycle to the next buffer
            ; Update stream pointer
            ld a, (ix + FILE_STREAM)
            add c
            ld (ix + FILE_STREAM), a

            bit 7, (ix + FILE_FLAGS)
            jr z, _
            ; Handle "last buffer"
            cp (ix + FILE_FINAL_LENGTH)
            jr nz, .loopAround
            set 5, (ix + FILE_FLAGS)
            jr .loopAround
_:          ; Handle any other buffer
            xor a
            cp (ix + FILE_STREAM)
            jr nz, .loopAround
            ld (ix + FILE_STREAM), a
            call getNextBuffer
            ld l, (ix + FILE_BUFFER)
            ld h, (ix + FILE_BUFFER + 1)
.loopAround:
            ; Back to the main loop
        pop bc
        jp .readLoop

;.done - 2:
        pop bc
.done:
    pop ix \ pop af \ pop de \ pop bc \ pop hl
    cp a
    ret

;; getStreamInfo [Filestreams]
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
        bit 5, (ix + FILE_FLAGS)
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
            bit 7, (ix + FILE_FLAGS)
            jr nz, _
            ld a, 0 ; 0x100 bytes
            jr ++_
_:          ld a, (ix + FILE_FINAL_LENGTH)
_:          ; Subtract amount already read from this block
            sub (ix + FILE_STREAM)
            ; And A is now the length left in this block
            ld c, a
            or a ; cp 0
            jr nz, _ \ inc b
_:          bit 7, (ix + FILE_FLAGS)
            jr nz, .done ; Leave eary for final block
            ; Loop through remaining blocks
            ld a, i
            push af \ di
                ld l, (ix + FILE_SECTION_ID)
                ld h, (ix + FILE_SECTION_ID + 1)
                ; HL is section ID
                dec b ; Reset B to zero
.loop:
                ld a, h
                setBankA
                ld a, l
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
                ; Check if it's 0xFFFE - if it is, we're done.
                ld a, 0xFF
                cp h
                jr nz, .continue
                cp l
                jr nz, .continue
                ; All done, add the final block length and exit
                ld a, (ix + FILE_FINAL_LENGTH)
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

;; streamReadToEnd [Filestreams]
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

; Seeks the stream (after getStreamEntry) to zero
; Destroys AF HL BC DE
seek_zero:
.loop:
    call selectSection
    ld h, 0x40
    add a, a \ add a, a
    ld l, a
    ld c, (hl)
    inc hl
    ld b, (hl)
    ld de, 0x7FFF
    call cpBCDE
    jr z, .resetPointer ; This is the first section
    res 7, (ix + FILE_FLAGS)
    ; Move on to next section
    ld (ix + FILE_SECTION_ID + 1), b
    ld (ix + FILE_SECTION_ID), c
    jr .loop
.resetPointer:
    ld (ix + FILE_STREAM), 0
    push ix
        ld b, (ix + FILE_SECTION_ID + 1)
        ld c, (ix + FILE_SECTION_ID)
        ld d, (ix + FILE_BUFFER + 1)
        ld e, (ix + FILE_BUFFER)
        push de \ pop ix
        call populateStreamBuffer
    pop ix
    ret

;; seek [Filestreams]
;;  Moves the current position in a file stream.
;; Inputs:
;;  D: Stream ID
;;  EBC: Offset to start of file in bytes
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
seek:
    call flush
    ret nz ; This nicely covers "stream not found"
    push ix
    push hl
    push de
    push af
    ld a, i
    push af
    di
        call getStreamEntry
        push bc \ push de
            call seek_zero
        pop de \ pop bc
.loop:
        ; Are we done?
        ld a, e
        or b
        jr z, .done
        ; Nope, not done. Reduce EB and swap in the next section
        ld a, b
        sub 1
        ld b, a
        jr nc, _
        ld a, e
        or a
        jr z, _
        dec e
_:      call selectSection
        add a, a \ add a, a
        ld h, 0x40
        ld l, 2
        add l
        ld l, a ; HL points to next block
        push bc \ push de
            ld c, (hl)
            inc hl
            ld b, (hl)
            ld de, 0x7FFF
            call cpBCDE
            jr z, .endOfStream
            ld (ix + FILE_SECTION_ID + 1), b
            ld (ix + FILE_SECTION_ID), c
        pop de \ pop bc
        jr .loop
.done:
        ld (ix + FILE_STREAM), c
        ld b, (ix + FILE_SECTION_ID + 1)
        ld c, (ix + FILE_SECTION_ID)
        ld d, (ix + FILE_BUFFER + 1)
        ld e, (ix + FILE_BUFFER)
        push de \ pop ix
        call populateStreamBuffer
    pop af
    jp po, _
    ei
_:  pop af
    pop de
    pop hl
    pop ix
    ret
.endOfStream:
        pop de \ pop bc
        ; Set us to the end of the stream
        ld c, (ix + FILE_FINAL_LENGTH)
        ld (ix + FILE_STREAM), c
        ld b, (ix + FILE_SECTION_ID + 1)
        ld c, (ix + FILE_SECTION_ID)
        ld d, (ix + FILE_BUFFER + 1)
        ld e, (ix + FILE_BUFFER)
        push de \ pop ix
        call populateStreamBuffer
    pop af
    jp po, _
    ei
_:  pop af
    pop de
    pop hl
    pop ix
    or 1
    ld a, errEndOfStream
    ret

;; getStreamPos [Filestreams]
;;  Returns the current position in a file stream.
;; Inputs:
;;  D: Stream ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code (on failure)
;;  EBC: Offset to start of file in bytes
; * Get section ID from File Stream Table
; * Using DAT linked list, count the number of sections until the beginning of the file
; * Add 256 bytes for each section and stream pointer from File Stream Table
getStreamPos:
    push ix
        call getStreamEntry
        jr z, .streamFound
    pop ix
    ret
.streamFound:
        push af
        push hl
        push de
            ; Initialize counter (EBC)
            ; ld de, 0 ; E must be zero but we need to use it first
            ld b, 0
            ld c, (ix + FILE_STREAM)
            ; Set pointer to first section ID
            ld de, FILE_SECTION_ID
            add ix, de
            ld de, 0
.loop:
            ; Set flash page
            ld a, (ix + 1)
            setBankA
            ; Get pointer to entry in linked list
            ; 0x4000 + 4*index
            ld hl, 0x4000
            ld l, (ix)
            rlc l
            rlc l
            push hl \ pop ix
            ; If entry is 0x7FFF, this is the beginning of the file
            ld a, (ix + 1)
            cp 0x7F
            jr nz, _
            ld a, (ix)
            cp 0xFF
            jr nz, _
            jr .exit
            ; Add section length to counter and loop
_:          ld hl, KFS_BLOCK_SIZE
            add hl, bc
            jr nc, _
            inc e
_:          push hl \ pop bc
            jr .loop
.exit:
            ex de, hl
        pop de
        ld e, l
        pop hl
        pop af
    pop ix
    ret
