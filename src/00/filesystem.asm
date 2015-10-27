; DEPRECATED
findFileEntry:
findDirectoryEntry:
    ret
; END DEPRECATED

; NOTE: Should we replace this with "deleteNode" and have it work on directories, too?

;; deleteFile [Filesystem]
;;  Deletes a file or symbolic link.
;; Inputs:
;;  DE: Path to file (string pointer)
;; Outputs:
;;  Z: Set if file was deleted, reset if file did not exist
;;  A: Preserved on success, error code on failure
deleteFile:
    push hl
    push af
        call findNode
        jr nz, .error
        push bc
            ld b, a
            ld a, i
            push af
                di
                ld a, b
                setBankA

                ; Check node type
                ld a, (hl)
                cp fsFile
                jr z, .file
                cp fsSymlink
                jr z, .symlink
                ; TODO: Directories?
                ld a, errNotAFile
                jr .error
.symlink:
                ld a, fsDeletedSymLink
                jr .delete
.file:
                ld a, fsDeletedFile
.delete:
                ; Perform actual deletion
                call unlockFlash
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
.error:
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
        call findNode ; TODO: Check if this node is a file
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
    jr fileExists ; TODO: Check if this node is a directory

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
        call findNode ; TODO: Check if this node is a directory
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
        cp fsSymLink
        jr z, .handleLink
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
.handleLink:
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
            cp fsFile
            jr z, .file
            cp fsSymLink
            jr z, .link
.directory: 
            ld bc, 5
            jr .add
.file:      
            ld bc, 8
            jr .add
.link:      
            ld bc, 3
.add:       
            or a
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

; createFileEntry [Internal]
;  Creates a new file entry in the FAT.
; Inputs:
;  HL: File name
;  DE: Parent ID
;  ABC: Length
;  IY: Section ID
; Outputs:
;  Z: Set on success, reset on failure
;  A: New entry Flash page (on success); Error code (on failure)
;  HL: New entry address relative to 0x4000 (on success)
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
        jp nz, .endOfFilesystem

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
            call strlen
            inc bc ; Zero delimited
        pop hl
        push bc
            ld a, 10
            add c
            ld c, a
            jr nc, _
            inc b
_:          call cpHLBC
        pop bc
        jp c, .endOfFilesystem
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
    push af
    push bc
    push de
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
    pop de
    pop bc
    pop af
    cp a
    ret
.exitError:
    pop de
    pop bc
    pop af
    or 1
    ret

; createDirectoryEntry [Internal]
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
            call strlen
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
    cp a
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
    call findNode
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
            call strlen
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
                call findNode ; TODO: Make sure this node is a directory
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
        jr nz, .error2
    inc sp \ inc sp ; pop hl
    pop de
    pop af
    jp po, _
    ei
_:  pop af
    cp a
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
    or 1
    ld a, errFileNotFound
    ret
        .error2:
    inc sp \ inc sp ; pop hl
    pop de
    pop af
    jp po, _
    ei
_:  pop af
    or 1
    ld a, errFilesystemFull
    ret

; findNode
;  Finds a node in the FAT.
; Inputs:
;  DE: Path to node (string pointer)
; Outputs:
;  Z: Set on success, reset on failure
;  A: Flash page (on success); error code (on failure)
;  HL: Address relative to 0x4000 (on success)
; Notes:
;  This works for any node - file, directory, symlink, etc. The rest of the
;  logic is up to you.
;  
;  '/' is a special node and does not actually exist in the filesystem. If you call
;  findNode with "/" you'll get HL=0.
findNode:
    push de
    push bc
    push af
    ld a, i
    push af
    di
.startOver:
        ; Skip initial / if present
        ; TODO: Allow for relative paths somehow
        ld a, (de)
        cp '/'
        jr nz, .proceed
        inc de
        ld a, (de)
        or a
        jr nz, .proceed
    ; Special case for /
    pop af
    jp po, _
    ei
_:  pop af
    ld a, b
    pop bc
    pop de
    cp a
    ld hl, 0
    ret
.proceed:
        setBankA(fatStart)
        ld hl, 0
        ld (kernelGarbage), hl ; Used as temporary storage of parent directory ID
        ld hl, 0x7FFF
        push af
            push de \ call checkForRemainingSlashes \ pop de
            jp z, findNode_fileLoop
_:          ld a, (hl)
            dec hl \ ld c, (hl) \ dec hl \ ld b, (hl) \ dec hl
            cp fsDirectory
            jr z, .handleDirectory
            cp fsSymLink ; TODO: Symlinks to directories?
            cp fsEndOfTable
            jr z, findNode_handleEndOfTable
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
                        call compareFileStrings
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
            jr findNode_fileLoop
findNode_handleEndOfTable:
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
findNode_fileLoop:
            ; Run once we've eliminated all slashes in the path
_:          ld a, (hl)
            dec hl \ ld c, (hl) \ dec hl \ ld b, (hl) \ dec hl
            cp fsFile
            jr z, .handleEntry
            cp fsDirectory
            jr z, .handleEntry
            cp fsSymLink
            jr z, .handleEntry
            cp fsEndOfTable
            jr z, findNode_handleEndOfTable
.continueSearch:
            or a
            sbc hl, bc
            jr -_
.handleEntry:
            push bc
                push hl
                    ; Check parent ID
                    ld c, (hl) \ dec hl \ ld b, (hl) \ dec hl
                    ld hl, (kernelGarbage)
                    call cpHLBC
                    jr z, .parentMatch
                pop hl
            pop bc
            jr .continueSearch
; These just load BC with the number of bytes to skip to get the node name
.fileMetadata:
    ld bc, 8
    ret
.dirMetadata:
    ld bc, 5
    ret
.linkMetadata:
    ld bc, 3
    ret
; Back to normal now
.parentMatch:
                pop hl \ push hl
                    cp fsFile
                    call z, .fileMetadata
                    cp fsDirectory
                    call z, .dirMetadata
                    cp fsSymLink
                    call z, .linkMetadata
                    scf \ ccf
                    sbc hl, bc
                    push de
                        call compareFileStrings
                    pop de
                pop hl
            pop bc
            jr nz, .continueSearch
            ; Return this entry, all done
            ld bc, 3
            add hl, bc
        pop bc ; pop af (with current FAT page)
    pop af
    jp po, _
    ei
_:  pop af
    ld a, b
    pop bc
    pop de
    cp a
    ret

;; fixKFS [Filesystem]
;;  Fixes issues found from [[checkFilesystem]]
fixKFS:
    ; TODO
    ret

;; checkFilesystem [Filesystem]
;;  Checks the integrity of the filesystem and sets certain bits of A with the results.
;; Outputs:
;;  A=0: No problems
;;  A/1: Unformatted pages found
;;  A/2: Invalid FAT entries found
;;  A/3: Invalid heap blocks found
;;  A/4: Unclosed new file entries found
;; Notes:
;;  Passing A into [[fixFilesystem]] is the appropriate course of action if A is nonzero.
checkKFS:
    ; TODO
    ret

; Checks string at (DE) for '/'
; Z for no slashes, NZ for slashes
; If the slash is the last character of the string, it's not considered
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
    inc de
    ld a, (de)
    or a
    ret

; Compare File Strings (HL is reverse)
; Z for equal, NZ for not equal
; Inputs: HL and DE are strings to compare
compareFileStrings:
    ld a, (de)
    or a
    jr z, .return
    cp ' ' ; TODO: Why is this here? Spaces are not file name delimiters
    jr z, .return
    cp '/'
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

;; renameFile [Filesystem]
;;  Renames a file.
;; Inputs:
;;  DE: Path to file (string pointer)
;;  HL: New filename
;; Outputs:
;;  Z: Set if the file was renamed, reset if file did not exist
;;  A: Preserved on success, error code on failure
renameFile:
    push hl \ push ix \ push bc \ push de \ push iy
        push af
            ld a, i
            push af
                push hl
                    ; Set the filetype to indicate the file's modified
                    call findNode ; TODO: Confirm that this is a file node
                    jr nz, .notFound
                    setBankA

                    di
                    ld a, fsModifiedFile
                    call unlockFlash
                    call writeFlashByte
                    call lockFlash

                    push hl \ pop ix
                pop hl
            
                ; Copy everything but the filename to a new entry
                ld e, (ix + -3)
                ld d, (ix + -4)
                ld c, (ix + -6)
                ld b, (ix + -7)
                ld a, (ix + -9) \ ld iyl, a
                ld a, (ix + -10) \ ld iyh, a
                ld a, (ix + -8)
                call createFileEntry
            pop af
            jp po, _
            ei
_:      pop af
        jr .done
.notFound:
               pop hl
               ld h, a
            pop af
            jp po, _
            ei
_:      pop af
        or 1
        ld a, h
.done:
    pop iy \ pop de \ pop bc \ pop ix \ pop hl
    ret

;; createSymLink [Filesystem]
;;  Creates a symbolic link.
;; Inputs:
;;  DE: Path to new symlink
;;  HL: Path to target
;; Outputs:
;;  Z: Set on success, reset on failure
;;  A: Flash page (on success); error code (on failure)
;;  HL: Address relative to 0x4000 (on success)
createSymLink:
    ; Check if node already exists
    ld a, 1
    or a    ; Reset Z
    push hl
        call findNode
        jr nz, _
    inc sp \ inc sp
    or 1
    ld a, errAlreadyExists
    ret
    ; Disable interrupts
_:  pop hl
    push af
    ld a, i
    push af
    di
    push bc
    push de
    push hl
    push ix
        ; push target
        push hl
            ; Seperate file path into dir and filename
            ; Inputs:
            ;  DE: Path string pointer
            ; Outputs:
            ;  BC: Pointer to last /
            push de
.loop:
                ld a, (de)
                or a
                jr z, .done
                cp '/'
                jr nz, _
                push de \ pop bc
_:              inc de
                jr .loop
.done:
            pop de
            ; push pointer to last /
            push bc
                ; get length of parent dir
                ld a, c
                sub e
                ld c, a
                ld a, b
                sbc d
                ld b, a
                ; copy parent dir to kernel garbage
                ; TODO: Make sure this spot is safe
                push de \ pop hl
                ld de, kernelGarbage + 0x100
                ldir
                xor a
                ld (de), a
                ; findNode parent dir, error if not found
                ld de, kernelGarbage + 0x100
                call findNode
                jp nz, .dirNotFound
                ; Begin creating new FS entry
                ; IX = buffer in kernel ram
                ld ix, kernelGarbage + 0x100
                ; Write symlink identifier
                ld (ix), fsSymLink
                dec ix
                ; Skip entry size until later
                dec ix
                dec ix
                ; write parent ID
                dec hl
                dec hl
                dec hl
                dec hl
                dec hl
                ld a, (hl)
                ld (ix), a
                dec hl
                dec ix
                ld a, (hl)
                ld (ix), a
                dec ix
            ; pop pointer to last /
            pop hl
            push hl \ pop de
            ; count length of filename
            ld bc, 0
_:          inc c
            inc de
            ld a, (de)
            or a
            jr nz, -_
            ; TODO: Error if length of filename is 0
            ; write length of filename
            ld (ix), c
            ; write filename
_:          inc hl
            dec ix
            ld a, (hl)
            ld (ix), a
            dec c
            ld a, c
            or a
            jr nz, -_
            dec ix
        ; pop and write target
        ; TODO: Error if target length is 0
        pop hl
_:      ld a, (hl)
        ld (ix), a
        dec ix
        inc hl
        or a
        jr nz, -_
        ; sub IX from start of buffer to get entry length
        push ix \ pop hl
        ld bc, kernelGarbage + 0x0FD
        ld a, c
        sub l
        ld c, a
        ld a, b
        sbc h
        ld b, a
        ; write entry length to buffer-1
        ld hl, kernelGarbage + 0x0FF
        ld (hl), c
        dec hl
        ld (hl), b
        ; get location of next entry in FS table
        call findFATEnd
        ; error if no more room in table
        jr nz, .fatFull
        ; subtract entry length from end of FAT
        inc bc
        inc bc
        inc bc
        ld a, l
        sub c
        ld l, a
        ld a, h
        sbc b
        ld h, a
        ; Check for end of flash page
        ; TODO: Stradle flash pages
        push bc
            ld bc, 0x4000
            call cpHLBC
        pop bc
        jp c, .fatFull
        ; write entry to flash
        call unlockFlash
        inc ix
        push ix \ pop de
        ex de, hl
        inc de
        call writeFlashBuffer
        call lockFlash
        ; Return HL with file entry address
        ex de, hl
        add hl, bc
        dec hl
        ; and return A with flash page
        getBankA
        ld (ix + 5), a
    pop ix
    inc sp \ inc sp ; Skip HL
    pop de
    pop bc
    ; Restore interrupts
    pop af
    jp po, _
    ei
_:  pop af
    cp a
    ret

.dirNotFound:
            ; pop pointer to last /
            pop bc
        ; pop target
        pop hl
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    jp po, _
_:  pop af
    or 1
    ld a, errFileNotFound
    ret

.fatFull:
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    jp po, _
_:  pop af
    or 1
    ld a, errFilesystemFull
    ret
