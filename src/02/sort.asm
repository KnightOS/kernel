;; integerSort [Miscellaneous]
;;  Sorts a specified array of 8-bit numbers using a fast (time complexity
;;  O(n)) algorithm.
;; Inputs:
;;  HL: first element in array
;;  DE: Last element in array
;; Notes:
;;  This routine is an in-place version of a radix sort, which has an O(k*n)
;;  runtime for k-bit numbers.  It also requires a smaller, fixed amount of
;;  stack space.
integerSort:
    ld b, 0b10000000
.recurse:
    push bc
        push de
            push hl
                or a                    ; We must initially clear CA bit, later it's never modified.
.nextbyte:
                sbc hl, de \ add hl, de ; Check if our bins have met up
                jr c, _
                jr nz, .nextbit         ; If they have, restart with next bit

_:              ld a, (hl)              ; Perform our bit test
                and b
                jr nz, _
                inc hl                  ; It's already in the 0s bin.  0s bin gets larger.
                jr .nextbyte
_:              ld a, (hl)              ; Switch number at top of 1s bin with this one
                ex de, hl
                ld c, (hl)
                ld (hl), a
                ex de, hl
                ld (hl), c
                dec de                  ; 1s bin gets larger.
                jr .nextbyte

.nextbit:
                srl b                   ; Next bit please
                jr c, .done             ; If our carry is 1, we've been through all 8 bits (base case).
            pop hl
            call .recurse               ; Sort the 0s bin
            ex de, hl
            inc hl
        pop de
        call .recurse                   ; Sort the 1s bin
    pop bc
    ret
.done:
            pop hl
        pop de
    pop bc
    ret

;; callbackSort [Miscellaneous]
;;  Sorts an array of arbitrarily-sized blocks using a callback function
;;  to perform comparisons.
;; Inputs:
;;  HL: First element in array
;;  DE: Last element in array
;;  BC: Size of element in bytes
;;  IX: Pointer to comparison function.
;; Notes:
;;  The comparison function must affect the carry flag like cp (hl), (de)
;;  would.  (That is, set the carry flag if (HL) < (DE).)  All other registers
;;  must be preserved.  The algorithm (quicksort) uses an average of O(log n)
;;  stack space, with 8 bytes stack per recursion required.  Quicksort is
;;  in-place and is not a stable sort.
callbackSort:
    ; Saves 4 bytes of stack per recursion
    call cpHLDE
    ret z
    push af
    push bc
        call .recurse
    pop bc
    pop af
    ret

.recurse:
    call cpHLDE
    ret z
    ret nc

    push iy
        ; middle = left
        push hl \ pop iy
        push hl
.loop:
            call .indirect ; cp (hl), (de)
            jr nc, _
            ; swap (HL) and (IY)
            call .swap
            ; "increment" middle
            add iy, bc
_:          add hl, bc
            call cpHLDE
            jr nz, .loop
        pop hl
        ; swap (IY) and (DE)
        ex hl, de
        call .swap
        ; recurse
        push iy
          ex (sp), hl
            xor a
            sbc hl, bc
            ex hl, de
            call .recurse
        pop de
        push iy
          ex (sp), hl
            add hl, bc
            call .recurse
        pop hl
    pop iy
    ret
.swap:
    push de
    push iy
    push hl
    push bc
_:      ld d, (hl)
        ld e, (iy)
        ld (iy), d
        ld (hl), e
        dec bc
        inc hl
        inc iy
        ld a, b \ or c
        jr nz, -_
    pop bc
    pop hl
    pop iy
    pop de
    ret
.indirect:
    jp (ix)
