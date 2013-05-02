; openFileRead 0007
test_openFileRead:
    xor a
    ld (currentThreadIndex), a
    ld (threadTable), a

    ld de, .testPath1
    call openFileRead
    jr nz, .fail
    xor a
    ld ix, fileHandleTable
    cp (ix)
    jr nz, .fail
    call closeStream
    dec a
    cp (ix)
    jr nz, .fail

    ld de, .testPath2
    call openFileRead
    jr nz, .fail
    xor a
    cp (ix)
    jr nz, .fail
    call closeStream

    ld de, .testPath3
    call openFileRead
    jr z, .fail
    ld b, a
    ld a, errFileNotFound
    cp b
    jr nz, .fail

    assert_pass()
.fail:
    assert_fail()
.testPath1:
    .db "/test.txt", 0
.testPath2:
    .db "/sub/test.txt", 0
.testPath3:
    .db "/does/not/exist", 0