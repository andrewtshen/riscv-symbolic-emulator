#lang rosette/safe

(require
	"init.rkt"
	"emulate.rkt"
	"execute.rkt"
	"machine.rkt"
	"pmp.rkt"
	"decode.rkt"
	"test.rkt")
(require benchmark)
(require (only-in racket/base for for/list for/vector in-range))
(require rackunit rackunit/text-ui)


(define (checkmem m m1 mmax)
	(printf "Amount of memory checked: ~a~n" mmax)

	(define-symbolic* sym-idx (bitvector 32))

	(define model_noninterference_with_sym_idx (verify
			#:assume
			(assert (and (bvule (bv #xF 32) sym-idx) (bvule sym-idx (bv (+ #xF mmax) 32))))
			#:guarantee
			(assert (equal? (memory-read (machine-ram m) sym-idx) (memory-read (machine-ram m1) sym-idx)))
			))
		(printf "model_noninterference: ~a~n" model_noninterference_with_sym_idx)
		(clear-asserts!))

(define ramsize #x10000)
(define m (init-machine ramsize))
(define m1 (deep-copy-machine m))
(define next_instr (step m)) ; step!

;; Example

; Benchmark a series of accesses
(for ([i (in-range 0 2000 100)])
	(time (checkmem m m1 i)))

; Benchmark a single access
; (time (checkmem m m1 10))

; ; Benchmark transitivity property
; (time (checkmem m m1 10))
