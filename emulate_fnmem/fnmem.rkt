#lang rosette/safe

(require
  "machine.rkt")
(require (only-in racket/file file->bytes)
     (only-in racket/base bytes-length for for/list in-range subbytes bytes-ref in-naturals))
(require syntax/parse/define)

; suppose addresses are 8-bit values
; and values are 32-bit values

; symbolic memory is a fn that maps 16-bit address ranges to 32-bit values
; mem is a symbolic uninterpreted fn
(define (new-symbolic-memory)
  (define-symbolic* mem (~> (bitvector 16) (bitvector 32)))
  mem)

(define all-zeros-memory
  (lambda (addr*)
    (if (< addr* 500)
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

(memory-read all-zeros-memory 1)
(memory-read all-zeros-memory 501)

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

;; memory bug

; start address is included, end address is not
(define (memory-write-range mem saddr eaddr value)
  (lambda (addr*)
    (if (and (<= saddr addr*) (< addr* eaddr))
      value
      (mem addr*))))
(provide memory-write-range)

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))

(define ramsize 2000)

(define mem1
  (lambda (addr*)
    (if (< addr* ramsize)
      ; (bv 0 8)
      (fresh-symbolic x (bitvector 8))
      null)))

(for [(i (in-range 0 ramsize))]
  (set! mem1 (memory-write mem1 i (fresh-symbolic x (bitvector 8)))))

(printf "mem1: ~a~n" (memory-read mem1 #x0))
(printf "mem1: ~a~n" (memory-read mem1 #x0))

(define mem2
  (lambda (addr*)
    (if (and (<= 0 addr*) (< addr* ramsize))
      ; (bv 0 8)
      (fresh-symbolic x (bitvector 8))
      null)))
(set! mem2 (memory-write-range mem2 0 2000 (fresh-symbolic x (bitvector 8))))
(set! mem2 (memory-write-range mem2 2000 4000 (fresh-symbolic x (bitvector 8))))

(printf "mem2: ~a~n" (memory-read mem2 #x0))
(printf "mem2: ~a~n" (memory-read mem2 #x0))


