#lang rosette/safe

(require
	"load.rkt"
	"decode.rkt"
	"execute.rkt"
	"machine.rkt")
(require (only-in racket/base for for/list in-range in-vector))

; Set up the machine and execute each instruction.
; Properties are proved at the end of the execution of the machine.

(define-syntax-rule (while test body ...) ; while loop
	(let loop () (when test body ... (loop))))

(define (print-program p)
	(for ([i (in-vector p)])
		(if (bvult i (bv 15 8))
			(printf "0")
			null)
		(printf "~a ~x " i (bitvector->natural i)))
	(printf "~n~n"))
(provide print-program)

(define (print-memory m start end)
	(for ([i (in-range start end)])
		(printf "~x: ~a~n" i (vector-ref (machine-ram m) i))))
(provide print-memory)

(define (print-csr m)
	(printf "mtvec ~a~n" (get-csr m "mtvec"))
	(printf "mepc ~a~n" (get-csr m "mepc"))
	(printf "pmpcfg0 ~a~n" (get-csr m "pmpcfg0"))
	(printf "pmpcfg2 ~a~n" (get-csr m "pmpcfg2"))
	(printf "pmpaddr0 ~a~n" (get-csr m "pmpaddr0"))
	(printf "pmpaddr1 ~a~n" (get-csr m "pmpaddr1"))
	(printf "pmpaddr2 ~a~n" (get-csr m "pmpaddr2"))
	(printf "pmpaddr3 ~a~n" (get-csr m "pmpaddr3"))
	(printf "pmpaddr4 ~a~n" (get-csr m "pmpaddr4"))
	(printf "pmpaddr5 ~a~n" (get-csr m "pmpaddr5"))
	(printf "pmpaddr6 ~a~n" (get-csr m "pmpaddr6"))
	(printf "pmpaddr7 ~a~n" (get-csr m "pmpaddr7"))
	(printf "pmpaddr8 ~a~n" (get-csr m "pmpaddr8")))
(provide print-csr)

; get instructions until reach mret
(define (test-and-execute m)
	(define op null)
	(while (not (equal? op "uret"))
		(define next_instr (decode (get-next-instr m)))
		(printf "PC: ~x INS: ~a~n" (get-pc m) next_instr)
		(set! op (list-ref next_instr 0))
		(execute next_instr m)))
(provide test-and-execute)

; ; example execution
; (define program (file->bytearray "build/sum.bin"))
; (printf-program)
; (printf "program: ~a~n" program)
; make machine
; (define ramsize 1000)
; (define m (init-machine program ramsize))
; (test-and-execute m)
; (print-memory m ramsize)
; (print-csr m)