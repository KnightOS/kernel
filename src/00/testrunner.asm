; Test runner for kernel unit tests

;Uncomment to automatically run the specified test
;.equ defaultTest 0x0007

;Uncomment to add a jr $ before running tests
;#define BREAK_BEFORE_TEST

; To create a new unit test:
; 1. Create a file for it in tests/ (if needed)
; 2. Add the test function
;    Return test success in A (0: Pass | 1: Fail)
; 3. Add test to test_collection
test_collection:
    .dw test_hexToA                         ; 0000 test_hexToA
    .dw test_cpHLDE                         ; 0001 test_cpHLDE
    .dw test_cpHLBC                         ; 0002 test_cpHLBC
    .dw test_cpBCDE                         ; 0003 test_cpBCDE
    .dw test_stringLength                   ; 0004 test_stringLength
    .dw test_DEMulA                         ; 0005 test_DEMulA
    .dw test_compareStrings                 ; 0006 test_compareStrings

    .dw test_openFileRead                   ; 0007 test_openFileRead
    .dw test_rleCompress                    ; 0008 test_rleCompress
    .dw test_rleDecompress                  ; 0009 test_rleDecompress
    .dw test_rleCalculateCompressedLength   ; 000A test_rlePredictCompress
    .dw test_rleCalculateDecompressedLength ; 000B test_rlePredictDecompress
    .dw test_crc16                          ; 000C test_crc16
    .dw test_sha1                           ; 000D test_sha1
    .dw test_integerSort                    ; 000E test_integerSort
    .dw test_callbackSort                   ; 000F test_callbackSort

    .dw 0xFFFF
explicit_only:
    ; Tests here are only run when explicity mentioned by number
test_collection_end:

testrunner:
    call getLcdLock
    call getKeypadLock
    call allocScreenBuffer
#ifdef defaultTest
    call clearBuffer
    ld hl, defaultTest * 2
    ld bc, test_collection
    or a
    adc hl, bc
    ld bc, defaultTest * 2
    ld e, (hl)
    inc hl
    ld d, (hl)
    dec hl
    ex de, hl
    jp testrunner_runtest
#endif
testrunner_continue:
    call clearBuffer
    ld hl, test_welcometext
    ld b, 0
    ld de, 0
    call drawStr
    call fastCopy
    ld bc, 6
    call malloc
    xor a
    call memset
    ld (ix + 5), 0xFF
    ld b, 0
.loop:
    call flushKeys
    call waitKey
    sub 9
    ld c, a
    ld hl, characterMap
    add hl, bc
    ld a, (hl)
    or a ; cp 0
    jr z, .loop
    cp '\n'
    jr z, .runselectedtest
    call drawChar
    call fastCopy
    ld (ix), a
    inc ix
    ld a, 0xFF
    cp (ix + 1)
    jr z, .runselectedtest
    jr .loop

.runselectedtest:
    call clearBuffer
    call memSeekToStart
    xor a
    cp (ix)
    jr z, testrunner_runall
    push ix \ pop hl
    call hexToHL
    add hl, hl
    push hl
        ld bc, test_collection
        or a
        adc hl, bc
        ld de, test_collection_end
        call cpHLDE
        jr nc, testrunner_nosuchtest
        ld e, (hl)
        inc hl
        ld d, (hl)
        dec hl
        ex de, hl
        ld bc, 0xFFFF
        call cpHLBC
        jr z, testrunner_nosuchtest
    pop bc
testrunner_runtest:
    ; Test exists, run it
    push hl
        ld de, 0
        ld hl, test_runningtext
        ld b, 0
        call drawStr
        push bc \ pop hl
        srl h \ rr l
        call drawHexHL
        ld a, '\n'
        call drawChar
    pop hl
    call fastCopy
    ld de, .return
    push de
#ifdef BREAK_BEFORE_TEST
    jr $
#endif
    jp (hl)
.return:
    or a
    jr nz, .failure
    ; Pass
    ld hl, test_passtext
    jp .displayresult
.failure:
    ld hl, test_failtext

.displayresult:
    ld de, 0x0006
    ld b, 0
    call drawStr
    ld hl, test_pressanykeytext
    call drawStr
    call fastCopy
    call flushKeys
    call waitKey
    jp testrunner_continue

testrunner_nosuchtest:
    ld de, 0
    ld hl, test_nosuchtesttext
    ld b, 0
    call drawStr
    call fastCopy
    call flushKeys
    call waitKey
    jp testrunner_continue

testrunner_runall:
    jr $ ; TODO

test_welcometext:
    .db "# Kernel unit test runner\n"
    .db "Press [Enter] to run all tests\n"
    .db "Or type a test number:\n", 0

test_nosuchtesttext:
    .db "Test not found\n"
test_pressanykeytext:
    .db "Press any key to continue", 0

test_runningtext:
    .db "Running test #", 0

test_passtext:
    .db "Result: PASS\n", 0
test_failtext:
    .db "Result: FAIL\n", 0

; Maps key codes onto everything you need to write hex test numbers
characterMap:
    .db '\n', 0, 0, 0, 0, 0, 0, 0
    .db 0, '3', '6', '9', 0, 0, 0, 0
    .db 0, '2', '5', '8', 0, 'F', 'C', 0
    .db '0', '1', '4', '7', 0, 'E', 'B', 0
    .db 0, 0, 0, 0, 0, 'D', 'A', 0

.macro assert_pass
    xor a
    ret
.endmacro

.macro assert_fail
    ld a, 1
    ret
.endmacro

#include "tests/util_tests.asm"
#include "tests/crypto_tests.asm"
#include "tests/filestreams_tests.asm"

.undefine assert_pass assert_fail
