#lang rosette/safe

(require
  "init.rkt"
  "emulate.rkt"
  "execute.rkt"
  "machine.rkt"
  "pmp.rkt"
  "decode.rkt"
  "test.rkt"
  "parameters.rkt")
; (require benchmark)
(require (only-in racket/base for for/list for/vector in-range))
(require rackunit rackunit/text-ui)


(define (checkmem m m1 mmax)
  (printf "Amount of memory checked: ~a~n" mmax)

  (define-symbolic* sym-idx (bitvector (ramsize-log2)))

  (define model_noninterference_with_sym_idx (verify
    #:assume
    ; use to test a range of values
    (assert (and (bvule (bv #x2000 (ramsize-log2)) sym-idx)
                 (bvule sym-idx (bv (+ #x2000 mmax) (ramsize-log2)))))
    ; ; use to test a certain value
    ; (assert (bveq sym-idx (bv #x2000 64)))
    #:guarantee
    ; (assert (equal? (memory-read (machine-ram m) sym-idx)
    ;                 (memory-read (machine-ram m1) sym-idx)))
    (assert-mem-equal m m1 sym-idx)))
  (printf "model_noninterference: ~a~n" model_noninterference_with_sym_idx)
  (clear-asserts!))

;; Example

; Benchmark a series of accesses
(for ([i (in-range 0 #x10000 #x1000)])
  (define m (init-machine))
  (define m1 (deep-copy-machine m))
  (define next_instr (step m))
  (time (checkmem m m1 i)))

; ; Benchmark a single access
; (define m (init-machine))
; (define m1 (deep-copy-machine m))
; (define next_instr (step m))
; (time (checkmem m m1 #x10000))

; ; Benchmark transitivity property
; (time (checkmem m m1 10))
