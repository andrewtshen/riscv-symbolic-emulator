#lang rosette/safe

(require (only-in racket/base for in-range))

; PMP utilities for decoding registers and checking

(define (ctz64 val)
  ; If bv with all zeros return 0, else ctz
  (cond
    [(bveq val (bv 0 64)) 0]
    [else
      (let helper ([i 0])
        (if (bveq (extract i i val) (bv 1 1))
          i
          (helper (+ 1 i))))]))
(provide ctz64)

; Decode R W X A settings for cfg register
(define (pmp-decode-cfg val idx)
	(define base (* idx 8))
	(define R (bitvector->natural (extract base base val)))
	(define W (bitvector->natural (extract (+ base 1) (+ base 1) val)))
	(define X (bitvector->natural (extract (+ base 2) (+ base 2) val)))
	(define A (bitvector->natural (extract (+ base 3) (+ base 3) val)))
	(list R W X A))
(provide pmp-decode-cfg)

; Decode start addr and end addr for cfg register
(define (pmp-decode-napot val)
	(define t1 (ctz64 (bvnot val)))
	(define base (bvshl (bvand val (bvnot (bvsub (bvshl (bv 1 64) (bv t1 64)) (bv 1 64)))) (bv 2 64)))
	(define range (bvsub (bvshl (bv 1 64) (bvadd (bv t1 64) (bv 3 64))) (bv 1 64)))
	(list base range))
(provide pmp-decode-napot)

(define (pmp-encode-napot base size)
	(define napot_size (bvsub (bvudiv size (bv 2 64)) (bv 1 64)))
	(define pmp_addr (bvlshr (bvadd base napot_size) (bv 2 64)))
	pmp_addr)

; Check if bv1 satisfies bv2 <= bv1 <= bv3
(define (bv-between bv1 bv2 bv3)
	(and (bvule bv2 bv1) (bvule bv1 bv3)))
(provide bv-between)

; (printf "base: #x80000000, size: #x20000 ~a~n" (pmp-encode-napot (bv #x80020000 64) (bv #x20000 64)))
; (printf "decoding #x: ~a~n" (pmp-decode-napot (bv #x7fffffffffffffff 64)))
; (printf "decoding #x: ~a~n" (pmp-decode-napot (bv #x00000000200003ff 64)))
