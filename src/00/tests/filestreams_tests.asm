test_openFileRead:
    xor a
    ld (currentThreadIndex), a
    ld (threadTable), a

    ld de, .testPath1
    call openFileRead
    jr nz, .fail
    ld a, 0b10000000
    ld ix, fileHandleTable
    cp (ix)
    jr nz, .fail
    call closeStream
    ld a, 0xFF
    cp (ix)
    jr nz, .fail

    ld de, .testPath2
    call openFileRead
    jr nz, .fail
    call closeStream

    ld de, .testPath3
    call openFileRead
    jr z, .fail
    call closeStream

    assert_pass()
.fail:
    assert_fail()
.testPath1:
    .db "/test.txt", 0
.testPath2:
    .db "/sub/test.txt", 0
.testPath3:
    .db "/does/not/exist", 0

test_closeStream:
    ld d, 0xFF
    call closeStream
    jr z, .fail

    ld de, .testPath1
    call openFileRead
    call getStreamBuffer
    call closeStream
    jr nz, .fail
    ; Confirm that memory has been freed
    ld a, 0xFF
    dec hl \ dec hl \ dec hl
    cp (hl)
    jr nz, .fail
    assert_pass()
.fail:
    assert_fail()
.testPath1:
    .db "/test.txt", 0

test_streamReadByte:
    ld d, 0xFF
    call streamReadByte
    jr z, .fail

    ld de, .testPath1
    call openFileRead
    call streamReadByte
    jr nz, .fail
    cp 'T'
    jr nz, .fail
    call streamReadByte
    jr nz, .fail
    cp 'e'
    jr nz, .fail

    call closeStream

    ; Test for end of stream
    ld de, .testPath1
    call openFileRead
    ld b, 10 ; size of test.txt
_:  call streamReadByte
    jr nz, .fail
    djnz -_
    call streamReadByte
    jr z, .fail
    call closeStream

    ; Test with file that is greater than one block in length
    ld b, 0xFF
    ld de, .testPath2
    call openFileRead
_:  call streamReadByte
    jr nz, .fail
    djnz -_
    call streamReadByte
    cp '\n'
    jr nz, .fail
    call streamReadByte
    cp 'B'
    jr nz, .fail
    ; Read up to next block
    ld b, 0xFF
_:  call streamReadByte
    jr nz, .fail
    djnz -_
    call streamReadByte
    cp 'e'
    jr nz, .fail
    ; Read last eleven bytes, then test for end of stream
    ld b, 11
_:  jr nz, .fail
    call streamReadByte
    jr nz, .fail
    djnz -_
    call streamReadByte
    jr z, .fail

    call closeStream

    assert_pass()
.fail:
    assert_fail()
.testPath1:
    .db "/test.txt", 0
.testPath2:
    .db "/large.txt", 0

test_streamReadWord:
    ; Since this one just uses streamReadByte, we can get away with minimal testing
    ld de, .testPath
    call openFileRead
    call streamReadWord
    ld a, 'T'
    cp l
    jr nz, .fail
    ld a, 'e'
    cp h
    jr nz, .fail

    call closeStream
    assert_pass()
.fail:
    assert_fail()
.testPath:
    .db "/test.txt", 0

test_streamReadBuffer:
    ; Test reading the beginning of a small file
    ld de, .testPath
    call openFileRead
    ld bc, 5
    call malloc
    dec bc
    call streamReadBuffer
    call closeStream
    xor a ; Terminate string
    ld (ix + 4), a
    push ix \ pop hl
    ld de, .testString
    call compareStrings
    jr nz, .fail
    ; Test the same, but with one byte already read
    ld de, .testPath
    call openFileRead
    call streamReadByte
    ld bc, 3
    call streamReadBuffer
    call closeStream
    xor a ; Terminate string
    ld (ix + 3), a
    push ix \ pop hl
    ld de, .testString + 1
    call compareStrings
    jr nz, .fail
    call free
    ; Test a larger file, with several blocks
    ld de, .testPath2
    call openFileRead
    ld bc, 0x101
    call malloc
    call streamReadBuffer
    call closeStream
    ld bc, 0x100
    add ix, bc
    ld a, (ix)
    cp 'B'
    jr nz, .fail
    assert_pass()
.fail:
    assert_fail()
.testPath:
    .db "/test.txt", 0
.testPath2:
    .db "/large.txt", 0
.testString:
    .db "Test", 0

test_getStreamInfo:
    ld de, .testPath
    call openFileRead
    call getStreamInfo
    call closeStream
    xor a
    cp b
    jr nz, .fail
    cp e
    jr nz, .fail
    ld a, 10
    cp c
    jr nz, .fail
    ld de, .testPath2
    call openFileRead
    call getStreamInfo
    call closeStream
    xor a
    cp e
    jr nz, .fail
    ld a, 524 >> 8
    cp b
    jr nz, .fail
    ld a, 524 & 0xFF
    cp c
    jr nz, .fail
    ld de, .testPath2
    call openFileRead
    call streamReadByte
    call getStreamInfo
    call closeStream
    xor a
    cp e
    jr nz, .fail
    ld a, 523 >> 8
    cp b
    jr nz, .fail
    ld a, 523 & 0xFF
    cp c
    jr nz, .fail
    assert_pass()
.fail:
    assert_fail()
.testPath:
    .db "/test.txt", 0 ; 10 bytes
.testPath2:
    .db "/large.txt", 0 ; 524 bytes

test_streamReadToEnd:
    ld de, .testPath
    call openFileRead
    push de
        call getStreamInfo
    pop de
    inc bc
    call malloc
    call streamReadToEnd
    call closeStream
    ld (ix + 10), 0 ; Terminate string
    push ix \ pop hl
    ld de, .expected
    call compareStrings
    jr nz, .fail
    call free
    ; Test large file
    ld de, .testPath2
    call openFileRead
    push de
        call getStreamInfo
    pop de
    call malloc
    call streamReadToEnd
    jr nz, .fail
    call closeStream
    call free
    ; Calculate SHA-1 of that file
    ;push ix \ pop hl
    ;call sha1Init
    ;call sha1AddRange
    ;call sha1Pad
    ;; Compare
    ;push ix \ pop de
    ;ld (ix + 20), 0 ; Terminate hash "string"
    ;ld hl, .largeFileSum
    ;call compareStrings
    ;jr nz, .fail
    ;call sha1Clean
    assert_pass()
.fail:
    assert_fail()
.testPath:
    .db "/test.txt", 0
.testPath2:
    .db "/large.txt", 0
.expected:
    .db "Test file!", 0
.largeFileSum:
    .db 0x7c, 0xdd, 0x93, 0x3a, 0x7a, 0x2d, 0x86, 0xbf
    .db 0xf0, 0x4d, 0xcf, 0x21, 0xcc, 0x86, 0x5f, 0xc2
    .db 0x18, 0xbf, 0x6e, 0x17, 0
