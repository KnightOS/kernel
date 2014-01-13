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
    assert(.test1, 3, .hash1)
    assert_pass()
.fail:
    call sha1Clean
    assert_fail()
.test1:
    .db "abc"
.hash1:
    .db 0xa9, 0x99, 0x3e, 0x36
    .db 0x47, 0x06, 0x81, 0x6a
    .db 0xba, 0x3e, 0x25, 0x71
    .db 0x78, 0x50, 0xc2, 0x6c
    .db 0x9c, 0xd0, 0xd8, 0x9d
.undefine assert
