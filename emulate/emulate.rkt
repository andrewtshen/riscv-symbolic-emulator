#lang rosette/safe

(require "load.rkt")
(require "decode.rkt")
(require "execute.rkt")

; Set up the machine and execute each instruction.
; Properties are proved at the end of the execution of the machine.

(define-syntax-rule (while test body ...) ; while loop
	(let loop () (when test body ... (loop))))

; get program
(define program (file->bitvectors "sum.bin"))

; make machine
(define ramsize 10)
(define m (init-machine program ramsize))

(define op null)
; get instructions until reach mret
(define (test-and-execute m)
	(while (not (equal? op "mret"))
		(define next_instr (decode (ram-get-x m (get-pc m))))
		(printf "PC: ~a INS: ~a~n" (get-pc m) next_instr)
		(set! op (list-ref next_instr 0))
		(execute next_instr m))
		
	; Begin proving properties
	(define x5 (gprs-get-x m 15))
	(define x6 (gprs-get-x m 16))
	(define x7 (gprs-get-x m 17))
	(printf "printing some registers...~n")
	(printf "~a~n" (gprs-get-x m 15))
	(printf "~a~n" (gprs-get-x m 16))
	(printf "~a~n" (gprs-get-x m 17))
	(define model-add (verify (begin 
		(assert (bveq (bvadd x5 x7) x6))
	)))
	(displayln model-add))

(test-and-execute m)
