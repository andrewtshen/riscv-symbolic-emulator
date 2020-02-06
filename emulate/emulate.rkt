#lang rosette/safe

(require
	"init.rkt"
	"decode.rkt"
	"execute.rkt"
	"machine.rkt"
	"pmp.rkt")
(require (only-in racket/base for in-range))

; Set up the machine and execute each instruction.
; Properties are proved at the end of the execution of the machine.

(define-syntax-rule (while test body ...) ; while loop
	(let loop () (when test body ... (loop))))

(define (print-memory m start end)
	(for ([i (in-range start end)])
		(printf "~x: ~a~n" i (vector-ref (machine-ram m) i))))
(provide print-memory)

(define (print-csr m)
	(printf "pc ~a~n" (get-pc m))
	(printf "mode ~a~n" (machine-mode m))
	(printf "mtvec ~a~n" (get-csr m 'mtvec))
	(printf "mepc ~a~n" (get-csr m 'mepc))
	(printf "pmpcfg0 ~a~n" (get-csr m 'pmpcfg0))
	(printf "pmpcfg2 ~a~n" (get-csr m 'pmpcfg2))
	(printf "pmpaddr0 ~a~n" (get-csr m 'pmpaddr0))
	(printf "pmpaddr1 ~a~n" (get-csr m 'pmpaddr1))
	(printf "pmpaddr2 ~a~n" (get-csr m 'pmpaddr2))
	(printf "pmpaddr3 ~a~n" (get-csr m 'pmpaddr3))
	(printf "pmpaddr4 ~a~n" (get-csr m 'pmpaddr4))
	(printf "pmpaddr5 ~a~n" (get-csr m 'pmpaddr5))
	(printf "pmpaddr6 ~a~n" (get-csr m 'pmpaddr6))
	(printf "pmpaddr7 ~a~n" (get-csr m 'pmpaddr7))
	(printf "pmpaddr8 ~a~n" (get-csr m 'pmpaddr8)))
(provide print-csr)

(define (step m)
	(define next_instr (decode m (get-next-instr m)))
	(execute next_instr m)
	next_instr)
(provide step)

; get instructions until reach mret
(define (execute-until-mret m)
	(define op null)
	(while (not (eq? op 'mret))
		(define next_instr (step m))
		; (printf "PC: ~x INS: ~a~n" (bitvector->natural (get-pc m)) next_instr)
		(set! op (list-ref next_instr 0))))
(provide execute-until-mret)

; ; example execution
; (define program (file->bytearray "build/pmp.bin"))
; (printf "~n* Running pmp.bin test ~n")
; (define ramsize 10000)
; (define m (init-machine-with-prog program ramsize))
; (execute-until-mret m)
