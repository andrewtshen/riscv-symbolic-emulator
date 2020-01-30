#lang rosette/safe

(require
	"load.rkt"
	"machine.rkt")
(require (only-in racket/base for in-range string-append number->string error))

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
		(printf "pmpaddr0 base/range: ~a~n" (pmp-decode-napot (get-csr m "pmpaddr0")))
		(printf "pmpaddr1 base/range: ~a~n" (pmp-decode-napot (get-csr m "pmpaddr1")))
		(printf "pmpaddr8 base/range: ~a~n" (pmp-decode-napot (get-csr m "pmpaddr8"))))
(provide print-pmp)

(define (ctz64 val)
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

; decode R W X A settings for cfg register
(define (pmp-decode-cfg val idx)
	(define base (* idx 8))
	(define R (bitvector->natural (extract base base val)))
	(define W (bitvector->natural (extract (+ base 1) (+ base 1) val)))
	(define X (bitvector->natural (extract (+ base 2) (+ base 2) val)))
	(define A (bitvector->natural (extract (+ base 3) (+ base 3) val)))
	(list A X W R))
(provide pmp-decode-cfg)

; decode start addr and end addr for cfg register
(define (pmp-decode-napot val)
	(define t1 (ctz64 (bvnot val)))
	(define base (bvshl (bvand val (bvnot (bvsub (bvshl (bv 1 64) (bv t1 64)) (bv 1 64)))) (bv 2 64)))
	; (printf "pmp-decode-napot base: ~a~n" base)
	; (printf "pmp-decode-napot base: ~x~n" (bitvector->integer base))
	(define range (bvsub (bvshl (bv 1 64) (bvadd (bv t1 64) (bv 3 64))) (bv 1 64)))
	; (printf "pmp-decode-napot range: ~a~n" range)
	(list base range))

; check if bv1 satisfies bv2 <= bv1 <= bv3
(define (bv-between bv1 bv2 bv3)
	(and (bvule bv2 bv1) (bvule bv1 bv3)))

(define (pmpcfg-check m pmpcfg saddr eaddr)
	(define legal #f)
	(define done #f)
	(for [(i (in-range 8))]
		#:break (equal? done #t)
		(define settings (pmp-decode-cfg pmpcfg i))
		; TODO check type of access
		(define R (list-ref settings 0))
		(define W (list-ref settings 1))
		(define X (list-ref settings 2))
		(define A (list-ref settings 3))
		(cond [(equal? A 1)
			; (printf "* Checking pmp~acfg~n" i)
			(define pmp_name (string-append "pmpaddr" (number->string i)))
			(define pmp (get-csr m pmp_name))
			(define pmp_bounds (pmp-decode-napot pmp))
			(define pmp_start (list-ref pmp_bounds 0))
			(define pmp_end (bvadd pmp_start (list-ref pmp_bounds 1)))

			(define slegal (bv-between saddr pmp_start pmp_end))
			(define elegal (bv-between eaddr pmp_start pmp_end))
			; if slegal #t and elegal #f, create illegal instruction
			; if elegal #f and slegal #t, create illegal instruction
			(cond
				[(and slegal (not elegal))
					(set! legal #f)]
				[(and elegal (not slegal))
					(set! legal #f)]
				[(and elegal slegal)
					(set! legal #t)
					(set! done #t)]
				[(and (not elegal) (not slegal))
					; Subcases
					; 1. both less than pmp_start -> continue testing other pmpcfgs
					; 2. both greater than pmp_start -> continue testing other pmpcfgs
					; 3. one below and one after -> illegal
					(cond
						[(and (bvult eaddr pmp_start) (bvult saddr pmp_end))
							null]
						[(and (bvult pmp_start eaddr) (bvult pmp_end saddr))
							null]
						[(and (bvult saddr pmp_start) (bvult pmp_end eaddr))
							(set! legal #f)
							(set! done #t)])]
				[else
					(error "Not possible decoding")])]))
	legal)

; test address ranging from saddr to eaddr 
(define (pmp-check m saddr eaddr)
	; check pmpcfg0, iterate through each register
	(define pmpcfg0 (get-csr m "pmpcfg0"))
	(define legal (pmpcfg-check m pmpcfg0 saddr eaddr))
	; check pmpcfg2, iterate through each register
	(cond
		[(not legal)
			(define pmpcfg2 (get-csr m "pmpcfg0"))
			(set! legal (pmpcfg-check m pmpcfg2 saddr eaddr))]))
	legal)
(provide pmp-check)