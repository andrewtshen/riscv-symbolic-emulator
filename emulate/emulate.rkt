#lang rosette/safe

(require "load.rkt")
(require "decode.rkt")
(require "execute.rkt")
(require (only-in racket/base for for/list in-range in-vector))

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
	(define gprsx
		(for/list ([i (in-range 10 18)])
			(gprs-get-x m i)))

	(printf "~nprinting some registers...~n")
	(for/list ([i gprsx])
			(printf "~a~n" i))
	(printf "proving add property~n")
	(define model-add (verify (begin 
		(assert (bveq (bvadd (list-ref gprsx 5) (list-ref gprsx 7))
									(list-ref gprsx 6))))))
	(displayln model-add)
	
	(printf "proving load and store~n")
	(define model-load-store (verify (begin 
		(assert (bveq (list-ref gprsx 2) (list-ref gprsx 3))))))
	(displayln model-load-store))

(test-and-execute m)

; (print-memory m ramsize)
(print-csr m)