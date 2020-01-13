#lang rosette/safe

(require "load.rkt")
(require "decode.rkt")
(require "execute.rkt")
(require (only-in racket/base for in-vector))

; Set up the machine and execute each instruction.
; Properties are proved at the end of the execution of the machine.

(define-syntax-rule (while test body ...) ; while loop
	(let loop () (when test body ... (loop))))

; get program
(define program (file->bytearray "sum.bin"))

(define (print-program p)
	(for ([i (in-vector program)])
		(if (bvult i (bv 15 8))
			(printf "0")
			null)
		(printf "~x " (bitvector->natural i)))
	(printf "~n~n"))
(print-program program)

; make machine
(define ramsize 1000)
(define m (init-machine program ramsize))

; (printf "program: ~a~n" program)
(define op null)
; get instructions until reach mret
(define (test-and-execute m)
	(printf "Running Program...~n")
	(while (not (equal? op "mret"))
		(define next_instr (decode (get-next-instr m)))
		(printf "PC: ~a INS: ~a~n" (get-pc m) next_instr)
		(set! op (list-ref next_instr 0))
		(execute next_instr m))
		
	; Begin proving properties
	(define x3 (gprs-get-x m 13))
	(define x4 (gprs-get-x m 14))
	(define x5 (gprs-get-x m 15))
	(define x6 (gprs-get-x m 16))
	(define x7 (gprs-get-x m 17))
	(printf "~nprinting some registers...~n")
	(printf "x2: ~a~n" (gprs-get-x m 12))
	(printf "x3: ~a~n" (gprs-get-x m 13))
	(printf "x4: ~a~n" (gprs-get-x m 14))
	(printf "x5: ~a~n" (gprs-get-x m 15))
	(printf "x6: ~a~n" (gprs-get-x m 16))
	(printf "x7: ~a~n" (gprs-get-x m 17))

	(printf "proving add property~n")
	(define model-add (verify (begin 
		(assert (bveq (bvadd x5 x7) x6))
	)))
	(displayln model-add))

(test-and-execute m)
