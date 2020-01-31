#lang rosette/safe

(require
	"load.rkt"
	"machine.rkt")
(require (only-in racket/base for in-range))

(define (print-special-regs m)
	(printf "mepc: ~a~n" (get-csr m "mepc"))
	(printf "mtvec: ~a~n" (get-csr m "mtvec"))
	(printf "mstatus: ~a~n" (get-csr m "mstatus")))
(provide print-special-regs)