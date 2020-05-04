#lang rosette/safe

(require (only-in racket/base for in-range))

; PMP utilities for decoding registers and checking

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
	(list R W X A))
(provide pmp-decode-cfg)

; decode start addr and end addr for cfg register
(define (pmp-decode-napot val)
	(define t1 (ctz64 (bvnot val)))
	(define base (bvshl (bvand val (bvnot (bvsub (bvshl (bv 1 64) (bv t1 64)) (bv 1 64)))) (bv 2 64)))
	(define range (bvsub (bvshl (bv 1 64) (bvadd (bv t1 64) (bv 3 64))) (bv 1 64)))
	(list base range))
(provide pmp-decode-napot)

; uint64_t get_pmp_napot_addr(uint64_t base, uint64_t size) {
;     uint64_t napot_size = ((size/2)-1);
;     uint64_t pmp_addr = (base + napot_size)>>2;
;     return pmp_addr;
; }

(define (pmp-encode-napot base size)
	(define napot_size (bvsub (bvudiv size (bv 2 64)) (bv 1 64)))
	(define pmp_addr (bvlshr (bvadd base napot_size) (bv 2 64)))
	pmp_addr)

; check if bv1 satisfies bv2 <= bv1 <= bv3
(define (bv-between bv1 bv2 bv3)
	(and (bvule bv2 bv1) (bvule bv1 bv3)))
(provide bv-between)

; (printf "base: #x80000000, size: #x20 ~a~n" (pmp-encode-napot (bv #x80000000 64) (bv #x10 64)))
; (printf "decoding #x20000bff: ~a~n" (pmp-decode-napot (bv #x0000000020000001 64)))
