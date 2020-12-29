#lang rosette/safe

(require (only-in racket/base for in-range))
(require syntax/parse/define)
(require (only-in racket/base build-vector))

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))

;; Structs to Build PMP

(struct pmp
  (pmpcfgs pmpaddrs num_implemented)
  #:mutable #:transparent)
(provide (struct-out pmp))

; pmpcfg struct
(struct pmpcfg
  (value settings)
  #:mutable #:transparent)
(provide (struct-out pmpcfg))

; pmpaddr struct
(struct pmpaddr
  (value start_addr end_addr)
  #:mutable #:transparent)
(provide (struct-out pmpaddr))

; pmpcfg settings setting
(struct pmpcfg_setting
  (R W X A L)
  #:mutable #:transparent)
(provide (struct-out pmpcfg_setting))

;; Helper Functions to Build PMP Structs

; Make a vector of pmpcfg settings
(define-simple-macro (make-pmpcfg_settings n:expr)
  (build-vector n (lambda (i) (define p (make-pmpcfg_setting)) p)))

; Make 1 pmpcfg setting
(define (make-pmpcfg_setting)
  (pmpcfg_setting
    (bv 0 1)
    (bv 0 1)
    (bv 0 1)
    (bv 0 2)
    (bv 0 1)))

(define (make-pmpcfg)
  (pmpcfg
    (bv 0 64)
    (make-pmpcfg_settings 8)))
(provide make-pmpcfg)

(define (make-pmpaddr)
  (pmpaddr
    (bv 0 64)
    (bv 0 64)
    (bv 0 64)))
(provide make-pmpaddr)

;; PMP utilities for decoding registers and checking

(define (get-pmpcfg-setting pmpcfg i)
  (vector-ref (pmpcfg-settings pmpcfg) i))
(provide get-pmpcfg-setting)

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

; TODO: Missing L bit for locking?
; Decode R W X A settings for cfg register
(define (pmp-decode-cfg val idx)
	(define base (* idx 8))
	(define R (extract base base val))
	(define W (extract (+ base 1) (+ base 1) val))
	(define X (extract (+ base 2) (+ base 2) val))
	(define A (extract (+ base 4) (+ base 3) val))
  (define L (extract (+ base 7) (+ base 7) val))
	(pmpcfg_setting R W X A L))
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
