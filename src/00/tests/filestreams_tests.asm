; openFileRead 0007
test_openFileRead:
    ; Test does not exist
    ld de, .testPath1
    call openFileRead
    jr z, .fail
    ld b, a
    ld a, errFileNotFound
    cp b
    jr nz, .fail

    ; Test stream creation
    ld de, .testPath2
    call openFileRead
    

    assert_pass()
.fail:
    assert_fail()
.testPath1:
    .db "/does/not/exist", 0
.testPath2:
    .db "/test.txt", 0
.testPath3:
    .db "/sub/test.txt", 0