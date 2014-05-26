;; deleteFile [Filesystem]
;;  Deletes a file.
;; Inputs:
;;  DE: Path to file (string pointer)
;; Outputs:
;;  Z: Set if file was deleted, reset if file did not exist
;;  A: Preserved on success, error code on failure
deleteFile:
    push hl
    push af
        call findFileEntry
        jr nz, ++_
        ; Delete file
        push bc
            ld b, a
            ld a, i
            push af
                di
                ld a, b
                setBankA
                call unlockFlash
                ld a, fsDeletedFile
                call writeFlashByte
                call lockFlash
            pop af
            jp po, _
            ei
_:      pop bc
    pop af
    pop hl
    cp a
    ret
_:  ; File not found
    ld h, a
    pop af
    or 1
    ld a, h
    pop hl
    ret

;; fileExists [Filesystem]
;;  Determines if a file exists.
;; Inputs:
;;  DE: Path to file (string pointer)
;; Outputs:
;;  Z: Set if file exists, reset if not
fileExists:
    push hl
    push af
        call findFileEntry
        jr nz, _
    pop af
    pop hl
    cp a
    ret
_:  ld h, a
    pop af
    or 1
    ld a, h
    pop hl
    ret

;; directoryExists [Filesystem]
;;  Determines if a directory exists.
;; Inputs:
;;  DE: Path to directory (string pointer)
;; Outputs:
;;  Z: Set if file exists, reset if not
directoryExists:
    push hl
    push af
        call findDirectoryEntry
        jr nz, _
    pop af
    pop hl
    cp a
    ret
_:  ld h, a
    pop af
    or 1
    ld a, h
    pop hl
    ret

;; listDirectory [Filesystem]
;;  Lists the contents of a directory on the filesystem.
;; Inputs:
;;  DE: Path to directory
;;  HL: Callback
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Error code on failure, preserved on success
;; Notes:
;;  This function will call your callback every time it encounters a
;;  relevant entry on the filesystem. You are free to use IX, IY, and
;;  the shadow registers in this callback, but must preserve all other
;;  registers. Your function will be called with the following state:
;;  
;;  * HL: Address of entry
;;  * BC: Length of entry
;;  * A: Type of entry
;;  * kernelGarbage: Name of entry
;;  * Correct page swapped into bank A
;;  * Interrupts disabled (do not enable them)
;;  
;;  You must leave these registers intact.
listDirectory:
    push af
    ld a, i
    push af
    di
    push de
    push bc
    push hl
    push hl
        call findDirectoryEntry
        ;jr nz, .error ; TODO
        ld bc, 0
        call cpHLBC
        call z, .correctForRoot
        jr z, .loop
        dec hl
        ld c, (hl)
        dec hl
        ld b, (hl)
        dec hl
        ; Grab directory ID
        dec hl \ dec hl
        ld e, (hl)
        dec hl
        ld d, (hl)
        ex de, hl
        ld (kernelGarbage + 0x100), hl
        ex de, hl
        inc hl \ inc hl
        getBankA
        ld (kernelGarbage + 0x102), a
        inc hl
        jr .skip
.loop:
        ld a, (hl)
        dec hl
        ld c, (hl)
        dec hl
        ld b, (hl)
        dec hl

        cp fsFile
        jr z, .handleFile
        cp fsDirectory
        jr z, .handleDirectory
        cp fsEndOfTable
        jp z, .endOfTable
.skip:
        or a
        sbc hl, bc
        ; TODO: Handle swapping out next page
        jr .loop
.handleFile:
.handleDirectory:
        push hl
            push bc
                ld c, (hl)
                dec hl
                ld b, (hl)
                ld hl, (kernelGarbage + 0x100)
                call cpHLBC
                jr z, .matchingItem
            pop bc
        pop hl
        jr .skip
.matchingItem:
            pop bc
            pop hl \ push hl
            cp fsDirectory
            jr nz, _
            ; Handle directory
            ld bc, 5
            jr ++_
_:          ; Handle file
            ld bc, 8
_:          or a
            sbc hl, bc ; Move HL to first character of name
            ld bc, kernelGarbage
_:          ld a, (hl)
            ld (bc), a
            inc bc
            dec hl
            or a
            jr nz, -_
        pop bc
        inc bc \ inc bc \ inc bc
        ld a, (bc)
        ld hl, .returnPoint
        ex (sp), hl
        push hl
        ld h, b \ ld l, c
    ret
.returnPoint:
    pop hl \ push hl
    push hl
        ld h, b
        ld l, c
        ld a, (kernelGarbage + 0x102)
        setBankA
        dec hl
        ld c, (hl)
        dec hl
        ld b, (hl)
        dec hl
        jp .skip
.endOfTable:
    pop hl
    pop hl
    pop bc
    pop de
    pop af
    jp po, _
    ei
_:  pop af
    cp a
    ret
.correctForRoot:
    ld (kernelGarbage + 0x100), hl
    ld hl, 0x7FFF
    ld a, fatStart
    setBankA
    ld (kernelGarbage + 0x102), a
    ret

;; createFileEntry [Filesystem]
;;  Creates a new file entry in the FAT.
;; Inputs:
;;  HL: File name
;;  DE: Parent ID
;;  ABC: Length
;;  IY: Section ID
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: New entry Flash page (on success); Error code (on failure)
;;  HL: New entry address relative to 0x4000 (on success)
createFileEntry:
    ; TODO: Check for file name too long
    push af
    ld a, i
    push af
    di
    push ix
    ld ix, 0
    add ix, sp
    push hl
    push de
    push bc
        call findFATEnd
        jr nz, .endOfFilesystem

.endOfTable:
        ; Write new entry here
        ld de, 0x3FFF
        scf
        push hl
            sbc hl, de
            ld b, h \ ld c, l
        pop hl
        ; BC is space left in this FAT page
        ld d, h \ ld e, l ; Save HL in DE
        ld h, (ix + -1)
        ld l, (ix + -2) ; Grab file name from stack
        push bc
            call stringLength
            inc bc ; Zero delimited
        pop hl
        push bc
            ld a, 10
            add c
            ld c, a
            jr nc, _
            inc c
_:          call cpHLBC
        pop bc
        jr c, .endOfFilesystem
        ; We're good to go, let's do this
        ; DE is the address in FAT (here, we'll modify it to be the end of the entry):
        ex de, hl
        scf
        sbc hl, bc
        push bc
            scf
            ld bc, 8
            sbc hl, bc
        pop bc
        ex de, hl
        ; BC is the length of the filename
        ; Everything else is on the stack
        ; Let's build a new entry in kernelGarbage and then write it all at once
        ld hl, kernelGarbage + 10
        add hl, bc
        ld a, fsFile 
        ld (hl), a ; Entry ID
        dec hl
        ; Increase BC to full length of entry for a moment
        push bc
            ld a, 8
            add c
            ld c, a
            jr nc, _
            inc c
_:          ; And write that length down
            ld (hl), c
            dec hl
            ld (hl), b
            dec hl
        pop bc
        ; Write parent ID
        ld a, (ix + -4)
        ld (hl), a
        dec hl
        ld a, (ix + -3)
        ld (hl), a
        dec hl
        ; Write flags (0xFF, someone else can modify it later if they want)
        ld a, 0xFF
        ld (hl), a
        dec hl
        ; File size
        ld a, (ix + -6)
        ld (hl), a
        dec hl
        ld a, (ix + -5)
        ld (hl), a
        dec hl
        ld a, (ix + 5)
        ld (hl), a
        dec hl
        ; Section ID
        push iy \ pop bc
        ld (hl), c
        dec hl
        ld (hl), b
        dec hl
        ld b, (ix + -1)
        ld c, (ix + -2) ; Grab file name from stack
        push de
            ld de, 0
.nameLoop:
            ld a, (bc)
            ld (hl), a
            dec hl
            inc bc
            inc de
            or a ; cp 0
            jr nz, .nameLoop
            ld b, d \ ld c, e
        pop de
        ; Grab full length of entry
        ld a, 11
        add c
        ld c, a
        jr nc, _
        inc b
_:      ; Write to flash
        inc hl
        call unlockFlash
        call writeFlashBuffer
        call lockFlash

        ex de, hl ; Return HL with file entry address
        add hl, bc
        dec hl
        getBankA
        ld (ix + 5), a ; And return A with Flash page
    pop bc
    pop de
    inc sp \ inc sp ; Skip HL
    pop ix
    pop af
    jp po, _
    ei
_:  pop af
    cp a
    ret

.endOfFilesystem:
    pop bc
    pop de
    pop hl
    pop af
    jp po, _
    ei
_:  pop af
    pop ix
    or 1
    ld a, errFilesystemFull
    ret

; Internal function - finds the end of the FAT, swaps that page in, and leaves HL for you to use
findFATEnd:
    ; Find end of FAT
    ld d, fatStart
    ld a, d
    setBankA
    ld hl, 0x7FFF
.search:
    ld a, (hl)
    cp fsEndOfTable
    jr z, .endOfTable
    dec hl
    ld c, (hl)
    dec hl
    ld b, (hl)
    scf
    sbc hl, bc ; Skip to next entry
    ld a, 0x40
    cp h
    jr c, .search
    ; Swap in next page of FAT
    dec d
    ld a, d
    cp fatStart - 4
    jp z, .exitError
    setBankA
    ld hl, 0x7FFF
    jr .search
.endOfTable:
    cp a
    ret
.exitError:
    or 1
    ret

; createDirectoryEntry [Filesystem] - Internal function
;  Creates a new directory entry in the FAT.
; Inputs:
;  HL: Directory name
;  DE: Parent ID
; Outputs:
;  Z: Set on success, reset on failure
;  A: New entry Flash page (on success); Error code (on failure)
;  HL: New entry address relative to 0x4000 (on success)
createDirectoryEntry:
    push ix
    push af
    ld a, i
    push af
    di
    push bc
    push de
    push hl
    ld ix, 0
    add ix, sp
        ; Traverse the FAT
        ld hl, 0
        ld (kernelGarbage + kernelGarbageSize - 2), hl ; Working directory ID
        ld a, fatStart
        setBankA
        ld hl, 0x7FFF
.search:
        ld a, (hl)
        cp fsEndOfTable
        jr z, .endOfTable

        dec hl
        ld c, (hl)
        dec hl
        ld b, (hl)
        dec hl

        cp fsDirectory
        jr z, .directory
.skip:
        or a
        sbc hl, bc ; Skip to next entry
        ld a, 0x40
        cp h
        jr c, .search
        ; Swap in next page of FAT
        dec d
        ld a, d
        cp fatStart - 4
        jp z, .exitError
        setBankA
        ld hl, 0x7FFF
        jr .search
.directory:
        dec hl \ dec hl
        ld e, (hl) \ dec hl
        ld d, (hl) \ dec hl
        push hl
            ld hl, (kernelGarbage + kernelGarbageSize - 2)
            or a
            sbc hl, de
        pop hl
        jr z, .update
        jr c, .update
        jr .search
.update:
        ex de, hl
        inc hl
        ld (kernelGarbage + kernelGarbageSize - 2), hl
        ex de, hl
        inc hl \ inc hl \ inc hl \ inc hl
        jr .skip
.endOfTable:
        ; HL: Address of next entry in FAT
        ; end of kernel garbage: New directory ID
        push hl
            ld l, (ix + 0)
            ld h, (ix + 1)
            call stringLength
            ld hl, 6
            add hl, bc
            ld b, h \ ld c, l
        pop hl
        ; BC: Length of new entry
        ld de, kernelGarbage
        ex de, hl
        add hl, bc
        inc hl \ inc hl
        ld a, fsDirectory
        ld (hl), a ; Entry type
        dec hl
        ld (hl), c ; Length of entry
        dec hl
        ld (hl), b ; cotd.
        dec hl
        ld c, (ix + 2)
        ld b, (ix + 3)
        ld (hl), c ; Parent ID
        dec hl
        ld (hl), b ; cotd.
        dec hl
        push hl
            ld hl, (kernelGarbage + kernelGarbageSize - 2)
            ld b, h \ ld c, l
        pop hl
        ld (hl), c ; New ID
        dec hl
        ld (hl), b ; cotd.
        dec hl
        ld a, 0xFF ; Flags
        ld (hl), a
        dec hl
        ld c, (ix + 0)
        ld b, (ix + 1) ; Grab file name from stack
        push de
            ld de, 0
.nameLoop:
            ld a, (bc)
            ld (hl), a
            dec hl
            inc bc
            inc de
            or a ; cp 0
            jr nz, .nameLoop
            ld b, d \ ld c, e
        pop de
        ; Set BC to full length of entry
        ld a, 8
        add c
        ld c, a
        jr nc, _
        inc b
_:      ; Move DE to end of file entry
        ex de, hl
        or a
        sbc hl, bc
        ex de, hl
        inc de
        ; Write new entry
        inc hl
        call unlockFlash
        call writeFlashBuffer
        call lockFlash
        ex de, hl ; HL has file entry address
        add hl, bc
        dec hl
        getBankA
        ld (ix + 7), a ; Flash page in A (on stack)
    inc sp \ inc sp ; Skip HL
    pop de
    pop bc
    pop af
    jp po, _
    ei
_:  pop af
    pop ix
    ret
.exitError:
    pop hl
    pop de
    pop bc
    pop af
    pop ix
    or 1
    ld a, errFilesystemFull
    ret

;; createDirectory [Filesystem]
;;  Creates a new directory in the filesystem and returns information about the new
;;  filesystem entry.
;; Inputs:
;;  DE: Path to new directory
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Flash page (on success); Error code (on failure)
;;  HL: Address relative to 0x4000 (on success)
createDirectory:
    call directoryExists
    call nz, fileExists ; TODO: Make a combined "entryExists" or something?
    jr nz, _
    or 1
    ld a, errAlreadyExists
    ret
_:  push af
    ld a, i
    push af
    di
    push de
    push hl
        ; Move string to RAM
        push bc
            ld h, d \ ld l, e
            ld bc, 0x8000
            scf
            sbc hl, bc
            jr nc, _ ; It's already in RAM
            ld h, d \ ld l, e
            call stringLength
            ld de, kernelGarbage + 10 ; + 10 gets us past what findDirectoryEntry uses
            ldir
            ld de, kernelGarbage + 10
_:      pop bc
        ld hl, 0
        ld a, (de)
        cp '/' ; Skip leading / (todo: working directories, see issue 75)
        jr nz, _
        inc de
_:      push de
            call checkForRemainingSlashes
        pop de
        ex de, hl
        jr z, .createRootEntry
        ; Find parent directory
        push bc
            xor a
            ld d, h \ ld e, l
            ld bc, 0
            cpir
            dec hl
            ld a, '/'
            ld bc, 0
            cpdr
            inc hl
            xor a
            ld (hl), a ; Remove final '/'
            push hl
                call findDirectoryEntry
                jr nz, .error
                ld bc, 4
                scf
                sbc hl, bc ; Skip some stuff we don't need
                ld e, (hl)
                dec hl
                ld d, (hl)
            pop hl
            ld a, '/'
            ld (hl), a ; Replace / in path
            inc hl ; HL is name of new directory
        pop bc
        jr .createEntry
.createRootEntry:
        ld de, 0
.createEntry:
        call createDirectoryEntry
    inc sp \ inc sp ; pop hl
    pop de
    pop af
    jp po, _
    ei
_:  pop af
    ret
                .error:
            pop hl
        pop bc
    pop hl
    pop de
    pop af
    jp po, _
    ei
_:  pop af
    ret

;; findFileEntry [Filesystem]
;;  Finds a file entry in the FAT.
;; Inputs:
;;  DE: Path to file (string pointer)
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Flash page (on success); Error code (on failure)
;;  HL: Address relative to 0x4000 (on success)
findFileEntry:
    push de
    push bc
    push af
    ld a, i
    push af
    di
        ; Skip initial / if present
        ; TODO: Allow for relative paths somehow
        ld a, (de)
        cp '/'
        jr nz, _
        inc de
_:      setBankA(fatStart)
        ld hl, 0
        ld (kernelGarbage), hl ; Used as temporary storage of parent directory ID
        ld hl, 0x7FFF
        push af
            push de \ call checkForRemainingSlashes \ pop de
            jp z, findFileEntry_fileLoop
_:          ld a, (hl)
            dec hl \ ld c, (hl) \ dec hl \ ld b, (hl) \ dec hl
            cp fsDirectory
            jr z, .handleDirectory
            cp fsSymLink ; TODO
            cp fsEndOfTable
            jr z, findFileEntry_handleEndOfTable
.continueSearch:
            or a
            sbc hl, bc
            ; TODO: Handle running off the page
            jr -_
.handleDirectory:
            push bc
                push hl
                    ld c, (hl) \ dec hl \ ld b, (hl)
                    ld hl, (kernelGarbage)
                    call cpHLBC
                    jr z, .compareNames
                    ; Not correct parent
                pop hl
            pop bc
            jr .continueSearch
.compareNames:
                    pop hl \ push hl
                    ld bc, 5
                    or a
                    sbc hl, bc
                    push de
                        call compareDirectories
                        jr z, .updateDirectory
                    pop de
                pop hl
            pop bc
            jr .continueSearch
.updateDirectory:
                    inc sp \ inc sp
                    inc de
                    push de \ call checkForRemainingSlashes \ pop de
                pop hl \ push hl
                    dec hl \ dec hl
                    ld c, (hl) \ dec hl \ ld b, (hl)
                    ld h, b \ ld l, c
                    ld (kernelGarbage), hl
                pop hl
            pop bc
            jr nz, .continueSearch
            or a
            sbc hl, bc
            jr findFileEntry_fileLoop
findFileEntry_handleEndOfTable:
        pop af
    pop af
    jp po, _
    ei
_:  pop af
    ld a, errFileNotFound
    or a ; Resets z
    pop bc
    pop de
    ret
findFileEntry_fileLoop:
            ; Run once we've eliminated all slashes in the path
_:          ld a, (hl)
            dec hl \ ld c, (hl) \ dec hl \ ld b, (hl) \ dec hl
            cp fsFile
            jr z, .handleFile
            cp fsSymLink ; TODO
            cp fsEndOfTable
            jr z, findFileEntry_handleEndOfTable
.continueSearch:
            or a
            sbc hl, bc
            jr -_
.handleFile:
            push bc
                push hl
                    ; Check parent directory ID
                    ld c, (hl) \ dec hl \ ld b, (hl)
                    ld hl, (kernelGarbage)
                    call cpHLBC
                    jr z, .compareNames
                    ; Not correct parent
                pop hl
            pop bc
            jr .continueSearch
.compareNames:
                pop hl \ push hl
                    ld bc, 8
                    or a
                    sbc hl, bc
                    push de
                        call compareFileStrings
                    pop de
                pop hl
            pop bc
            jr z, .fileFound
            jr .continueSearch
.fileFound:
            ld bc, 3
            add hl, bc
        pop bc ; pop af
    pop af
    jp po, _
    ei
_:  pop af
    ld a, b
    pop bc
    pop de
    cp a
    ret

;; findDirectoryEntry [Filesystem]
;;  Finds a directory entry in the FAT.
;; Inputs:
;;  DE: Path to directory
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Flash page (on success); Error code (on failure)
;;  HL: Address relative to 0x4000 (on success)
;; Notes:
;;  This function returns HL=0 for the root directory. You should
;;  handle this special case yourself. The root directory has ID 0
;;  and has no parent directory.
findDirectoryEntry:
    push de
    push bc
    push af
    ld a, i
    push af
    di
        ; TODO: Relative paths
        setBankA(fatStart)
        ld hl, 0
        ld (kernelGarbage), hl ; Parent ID
        ld hl, 0x7FFF
.search:
        ld a, (de)
        cp '/'
        jr nz, _
        inc de
        ld a, (de)
_:      or a ; cp 0
        jr z, .done_root ; Root is a special case
.traversalLoop:
        ; Traverse the table
        ld a, (hl)
        dec hl
        ld c, (hl)
        dec hl
        ld b, (hl)
        dec hl

        cp fsDirectory
        jr z, .handleDirectory
        cp fsSymLink ; TODO
        cp fsEndOfTable
        jr z, .endOfTable
_:
        or a
        sbc hl, bc
        ; TODO: Handle swapping out next page
        jr .traversalLoop
.skip:
        inc hl
        inc hl
        inc hl
        inc hl
        inc hl
        jr -_
.handleDirectory:
        ; Check parent IDs
        push bc
        push hl
            ld c, (hl)
            dec hl
            ld b, (hl)
            dec hl
            push hl
                ld hl, (kernelGarbage)
                call cpHLBC
            pop hl
            jr z, _
        pop hl
        pop bc
        jr -_
_:          ; Parent IDs match, check name
            ld c, (hl)
            dec hl
            ld b, (hl) ; Grab new directory ID just in case
            dec hl
            dec hl ; Skip flags
            push de
                call compareDirectories
                jr z, _
            pop de
        inc sp \ inc sp ; pop hl
        pop bc
        jr .skip
_:          inc sp \ inc sp ; pop de
            ; Directories match, update kernelGarbage
            ld (kernelGarbage), bc
            ; We might be done here
            ld a, (de)
            cp '/'
            jr nz, _
            inc de
            ld a, (de)
_:          or a ; cp 0
            jr nz, .continue
            ; We are done!
        pop hl
        pop bc
        ld bc, 3
        add hl, bc
        jr .done
.continue:
        inc sp \ inc sp ; pop hl
        pop bc
        dec hl
        jr .traversalLoop
.endOfTable:
    pop af
    jp po, _
    ei
_:  pop af
    pop bc
    pop de
    or 1
    ld a, errFileNotFound
    ret
.done_root:
        ld hl, (kernelGarbage)
.done:
        getBankA
        ld b, a
    pop af
    jp po, _
    ei
_:  pop af
    ld a, b
    pop bc
    pop de
    cp a
    ret

;; formatUnusedPages [Filesystem]
;;  If /bin/init is called with A set to 0xFF, this function
;;  should be called. It may be appropriate to show the user
;;  some sort of UI while this is processing, as it will take
;;  some time to run.
;; Notes:
;;  This function is only relevant to system-level programmers.
;;  Most programmers do not have to concern themselves with it.
formatUnusedPages:
    ; TODO
    ret

; Checks string at (DE) for '/'
; Z for no slashes, NZ for slashes
checkForRemainingSlashes:
    ld a, (de)
    or a ; CP 0
    ret z
    cp '/'
    jr z, .found
    inc de
    jr checkForRemainingSlashes
.found:
    ; Check for one last slash
    or a
    ret

; Compare string, but also allows '/' as a delimiter.  Also compares HL in reverse.
; Also allows for paths to have a trailing '/'
; Z for equal, NZ for not equal
; HL = backwards string
; DE = fowards string
compareDirectories:
    ld a, (de)
    or a
    jr z, .return
    cp '/'
    jr z, .return
    cp ' '
    jr z, .return
    cp (hl)
    ret nz
    dec hl
    inc de
    jr compareDirectories
.return:
    ld a, (hl)
    or a
    ret

; Compare File Strings (HL is reverse)
; Z for equal, NZ for not equal
; Inputs: HL and DE are strings to compare
compareFileStrings:
    ld a, (de)
    or a
    jr z, .return
    cp ' '
    jr z, .return
    cp (hl)
    ret nz
    dec hl
    inc de
    jr compareFileStrings
.return:
    ld a, (hl)
    or a
    ret
