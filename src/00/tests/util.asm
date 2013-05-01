; hexToA 0000
.macro assert(testString, expected)
    ld hl, testString
    call hexToA
    cp expected
    jr nz, .fail
.endmacro
test_hexToA:
    assert(.test1, 0x00)
    assert(.test2, 0xFF)
    assert(.test3, 0x10)
    assert(.test4, 0xA4)
    assert_pass()
.fail:
    assert_fail()
.test1:
    .db "00", 0
.test2:
    .db "FF", 0
.test3:
    .db "10", 0
.test4:
    .db "A4", 0
.undefine assert

; cpHLDE 0001
test_cpHLDE:
    ld hl, 0
    ld de, 20
    call cpHLDE
    jr z, .fail
    jr nc, .fail
    assert_pass()
.fail:
    assert_fail()

; cpHLBC 0002
test_cpHLBC:
    ld hl, 0
    ld bc, 20
    call cpHLBC
    jr z, .fail
    jr nc, .fail
    assert_pass()
.fail:
    assert_fail()

; cpBCDE 0003
test_cpBCDE:
    ld bc, 0
    ld de, 20
    call cpBCDE
    jr z, .fail
    jr nc, .fail
    assert_pass()
.fail:
    assert_fail()

; stringLength 0004
.macro assert(testString, expected)
    ld hl, testString
    call stringLength
    ld a, expected & 0xFF
    cp c
    jr nz, .fail
    ld a, (expected & 0xFF00) >> 8
    cp b
    jr nz, .fail
.endmacro
test_stringLength:
    assert(.test1, 0)
    assert(.test2, 4)
    assert(.test3, 26)
    assert(.test4, 256)
    assert(.test5, 320)
    assert_pass()
.fail:
    assert_fail()
.test1:
    .db 0
.test2:
    .db "four", 0
.test3:
    .db "abcdefghijklmnopqrstuvwxyz", 0
.test4:
    .db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    .db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    .db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    .db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0
.test5:
    .db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    .db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    .db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    .db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    .db "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0
.undefine assert

; DEMulA 0005
.macro assert(valDE, valA)
    ld de, valDE
    ld a, valA
    call DEMulA
    ld a, (valDE * valA) & 0xFF
    cp l
    jr nz, .fail
    ld a, ((valDE * valA) & 0xFF00) >> 8
    cp h
    jr nz, .fail
.endmacro
test_DEMulA:
    assert(10, 20)
    assert(2, 2)
    assert(0x1000, 2)
    assert(0x1234, 2)
    assert(1234, 0)
    assert_pass()
.fail:
    assert_fail()
.undefine assert

; compareStrings 0006
.macro assert_equal(stringA, stringB)
    ld hl, stringA
    ld de, stringB
    call compareStrings
    jr nz, .fail
.endmacro
.macro assert_notequal(stringA, stringB)
    ld hl, stringA
    ld de, stringB
    call compareStrings
    jr z, .fail
.endmacro
test_compareStrings:
    assert_equal(.string1, .string1)
    assert_equal(.string1, .string2)
    assert_notequal(.string1, .string3)
    assert_notequal(.string1, .string4)
    assert_notequal(.string1, .string5)
    assert_pass()
.fail:
    assert_fail()
.string1:
    .db "equal", 0
.string2:
    .db "equal", 0
.string3:
    .db "notequal", 0
.string4:
    .db "ne", 0
.string5:
    .db 0
.undefine assert_equal assert_notequal