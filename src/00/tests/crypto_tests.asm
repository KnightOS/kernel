; crc16 000C
.macro assert(testString, size, expected)
    ld hl, testString
    ld bc, size
    call crc16
    ld hl, expected
    call cpHLDE
    jr nz, .fail
.endmacro
test_crc16:
    assert(.test1, 45, 0x349F)
    assert(.test2,  1, 0xE1F0)
    assert_pass()
.fail:
    assert_fail()
.test1:
    .db "The quick brown fox jumps over the lazy dog."
.test2:
    .db 0
.undefine assert

; sha1 000D
.macro assert(dataPtr, dataSize, expectedHashPtr)
    call sha1Init
    ld hl, dataPtr
    ld bc, dataSize
    call sha1AddRange
    call sha1Pad

    push ix \ pop de
    ld b, 20
    ld hl, expectedHashPtr
_:  ld a, (de)
    cp (hl)
    inc hl
    inc de
    jr nz, .fail
    djnz -_
    call sha1Clean
.endmacro

test_sha1:
    assert(.test1, 43, .hash1)
    assert_pass()
.fail:
    call sha1Clean
    assert_fail()
.test1:
    .db "The quick brown fox jumps over the lazy dog"
.hash1:
    .db 0x2f, 0xd4, 0xe1, 0xc6
    .db 0x7a, 0x2d, 0x28, 0xfc
    .db 0xed, 0x84, 0x9e, 0xe1
    .db 0xbb, 0x76, 0xe7, 0x39
    .db 0x1b, 0x93, 0xeb, 0x12
.undefine assert
