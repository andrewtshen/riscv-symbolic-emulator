#lang rosette/safe

(require
	"load.rkt"
	"machine.rkt")
(require (only-in racket/base for in-range))

(define (print-pmp m)
		(printf "pmpcfg0: ~a~n" (get-csr m "pmpcfg0"))
		(printf "pmpcfg2: ~a~n" (get-csr m "pmpcfg2"))
		(printf "pmpaddr0: ~a~n" (get-csr m "pmpaddr0"))
		(printf "pmpaddr1: ~a~n" (get-csr m "pmpaddr1"))
		(printf "pmpaddr2: ~a~n" (get-csr m "pmpaddr2"))
		(printf "pmpaddr3: ~a~n" (get-csr m "pmpaddr3"))
		(printf "pmpaddr4: ~a~n" (get-csr m "pmpaddr4"))
		(printf "pmpaddr5: ~a~n" (get-csr m "pmpaddr5"))
		(printf "pmpaddr6: ~a~n" (get-csr m "pmpaddr6"))
		(printf "pmpaddr7: ~a~n" (get-csr m "pmpaddr7"))
		(printf "pmpaddr8: ~a~n" (get-csr m "pmpaddr8"))
		(printf "pmpaddr0 base/range~n")
		(pmp_decode_napot (get-csr m "pmpaddr0"))
		(printf "pmpaddr1 base/range~n")
		(pmp_decode_napot (get-csr m "pmpaddr1"))
		(printf "pmpaddr8 base/range~n")
		(pmp_decode_napot (get-csr m "pmpaddr8")))

(provide print-pmp)

(define (ctz64 val)
	(printf "ctz64 val: ~a~n" val)
	(define numz 0)
	(cond
		[(bveq val (bv 0 64))
			(set! numz 0)]
		[else
			; iterate through the bitvector and stop on first 1
			(for [(i (in-range 0 64))]
				#:break (bveq (extract numz numz val) (bv 1 1))
				(set! numz (+ numz 1)))])
	numz)
(provide ctz64)

(define (pmp_decode_napot val)
	(define t1 (ctz64 (bvnot val)))
	(define base (bvshl (bvand val (bvnot (bvsub (bvshl (bv 1 64) (bv t1 64)) (bv 1 64)))) (bv 2 64)))
	(printf "t1: ~a~n" t1)
	(printf "pmp_decode_napot base: ~a~n" base)
	(printf "pmp_decode_napot base: ~x~n" (bitvector->integer base))
	(define range (bvsub (bvshl (bv 1 64) (bvadd (bv t1 64) (bv 3 64))) (bv 1 64)))
	(printf "pmp_decode_napot range: ~a~n" range)
	(list base range))
