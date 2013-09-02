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
