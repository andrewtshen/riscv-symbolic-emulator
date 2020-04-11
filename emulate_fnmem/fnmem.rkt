#lang rosette/safe

(require
  "machine.rkt")

; suppose addresses are 8-bit values
; and values are 32-bit values

; symbolic memory is a fn that maps 16-bit address ranges to 32-bit values
; mem is a symbolic uninterpreted fn
(define (new-symbolic-memory)
  (define-symbolic* mem (~> (bitvector 16) (bitvector 32)))
  mem)

(define all-zeros-memory
  (lambda (addr*)
    (if (bvslt addr* (bv #x2000 16))
      (bv 0 32)
      (bv -1 32))))

; applies the uninterpreted fn mem to addr, which returns some value
(define (memory-read mem addr)
  (mem addr))

; returns a function which, if the (addr*) is equal to the
; the (addr) then it returns the stored value, otherwise, it
; processes (mem addr*) instead.
(define (memory-write mem addr value)
  (lambda (addr*)
    (if (equal? addr addr*)
      value
      (mem addr*))))


;; examples

(memory-read all-zeros-memory (bv #b01010000 16))
(memory-read all-zeros-memory (bv #x2001 16))

(memory-read
 (memory-write all-zeros-memory (bv #x0000 16) (bv #x1337 32))
 (bv #x0000 16))

;; examples with symbolic memory

; this allows us to make "blank"/symbolic memory
(define init-mem (new-symbolic-memory))

(memory-read
 init-mem
 (bv #x00 16))

(memory-read
 (memory-write init-mem (bv #x0000 16) (bv #x1337 32))
 (bv #x00 16))

(define-symbolic* addr (bitvector 16))

(verify
 #:assume (assert (not (equal? addr (bv 0 16))))
 #:guarantee
 (assert
  (equal?
   (memory-read
    (memory-write init-mem (bv #x0000 16) (bv #x1337 32))
    addr)
   (memory-read init-mem addr))))