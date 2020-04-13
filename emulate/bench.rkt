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
	(define model_noninterference (verify (begin
		(assert-csr-equal m m1) ; check all the relevant csrs values

		; show that all the memory in 0 - 0x2000 can't change
		(for ([i (in-range #x0 mmax)])
			(assert (bveq (vector-ref (machine-ram m) i) (vector-ref (machine-ram m1) i))))

		; (printf "assert: ~a~n" (asserts))
		)))
	(printf "res: ~a~n" model_noninterference))

(define ramsize 15000)
(define m (init-machine ramsize))
(define m1 (deep-copy-machine m))
(define next_instr (step m)) ; step!

; show that they can execute independently, but
; still refer to the same symbolic variables.
(for ([i (in-range 0 1000 100)])
	(time (checkmem m m1 i)))

; (time (checkmem m m1 1))