#lang rosette/safe

(require "load.rkt")
(require "decode.rkt")
(require "execute.rkt")
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
		(printf "~x " (bitvector->natural i)))
	(printf "~n~n"))

(define (print-memory m ramsize)
	(for ([i (machine-ram m)])
		(printf "~a " i))
	(printf "~n~n"))

(define (print-csr m)
	(printf "ustatus: ~a~n" (get-csr m "ustatus"))
	(printf "uie: ~a~n" (get-csr m "uie"))
	(printf "utvec: ~a~n" (get-csr m "utvec"))
	(printf "uscratch: ~a~n" (get-csr m "uscratch"))
	(printf "uepc: ~a~n" (get-csr m "uepc"))
	(printf "ucause: ~a~n" (get-csr m "ucause"))
	(printf "ubadaddr: ~a~n" (get-csr m "ubadaddr"))
	(printf "uip: ~a~n" (get-csr m "uip"))
	(printf "mstatus: ~a~n" (get-csr m "mstatus"))
	(printf "misa: ~a~n" (get-csr m "misa"))
	(printf "medeleg: ~a~n" (get-csr m "medeleg"))
	(printf "mideleg: ~a~n" (get-csr m "mideleg"))
	(printf "mie: ~a~n" (get-csr m "mie"))
	(printf "mtvec: ~a~n" (get-csr m "mtvec"))
	(printf "mscratch: ~a~n" (get-csr m "mscratch"))
	(printf "mepc: ~a~n" (get-csr m "mepc"))
	(printf "mcause: ~a~n" (get-csr m "mcause"))
	(printf "mbadaddr: ~a~n" (get-csr m "mbadaddr"))
	(printf "mip: ~a~n" (get-csr m "mip")))

(define op null)
; get instructions until reach mret
(define (test-and-execute m)
	(printf "Running Program...~n")
	(while (not (equal? op "mret"))
		(define next_instr (decode (get-next-instr m)))
		(printf "PC: ~a INS: ~a~n" (get-pc m) next_instr)
		(set! op (list-ref next_instr 0))
		(execute next_instr m))

	(for/list ([i (in-range 10 18)])
			(printf "~a~n" (gprs-get-x m i))))
(provide test-and-execute)


; ; get program
; (define program (file->bytearray "sum.bin"))
; (printf-program)
; (printf "program: ~a~n" program)
; make machine
; (define ramsize 1000)
; (define m (init-machine program ramsize))
; (test-and-execute m)
; (print-memory m ramsize)
; (print-csr m)