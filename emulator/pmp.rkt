#lang rosette/safe

(require (only-in racket/base for in-range))
(require syntax/parse/define)
(require (only-in racket/base build-vector))

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))

;; Structs to Build PMP

(struct pmp
  (pmpcfgs pmpaddrs)
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
(struct pmpcfg-setting
  (R W X A L)
  #:mutable #:transparent)
(provide (struct-out pmpcfg-setting))

;; Helper Functions to Build PMP Structs

; Make a vector of pmpcfg settings
(define-simple-macro (make-pmpcfg-settings n:expr)
  (build-vector n (lambda (i) (define p (make-pmpcfg-setting)) p)))

; Make 1 pmpcfg setting
(define (make-pmpcfg-setting)
  (pmpcfg-setting
    (fresh-symbolic R (bitvector 1))
    (fresh-symbolic W (bitvector 1))
    (fresh-symbolic X (bitvector 1))
    (fresh-symbolic A (bitvector 2))
    (fresh-symbolic L (bitvector 1))))

(define (make-pmpcfg)
  (pmpcfg
    (fresh-symbolic value (bitvector 64))
    (make-pmpcfg-settings 8)))
(provide make-pmpcfg)

(define (make-pmpaddr)
  (pmpaddr
    (fresh-symbolic value (bitvector 64))
    (fresh-symbolic start_addr (bitvector 64))
    (fresh-symbolic end_addr (bitvector 64))))
(provide make-pmpaddr)

;; PMP checks

; (define (pmpcfg-check m pmpcfg saddr eaddr pmpaddrs)
;   (define legal null)
;   (define done #f)
;   ; somewhat hacky way of getting right regs, doesn't work if id odd
;   ; but this is okay because for risc-V 64, always id even
;   (for ([i (in-range 0 8)]
;         [pmp_name pmpaddrs]
;         #:break (equal? done #t))

;     (define settings (pmp-decode-cfg pmpcfg i))

;     ; TODO: Implement check type of access
;     (define R (list-ref settings 0))
;     (define W (list-ref settings 1))
;     (define X (list-ref settings 2))
;     (define A (list-ref settings 3))
;     ; This is case Top Of Range (TOP) encoding
;     ; TODO: Implement different types of PMP encodings
;     (when (equal? A 1)
;       (define pmp (get-csr m pmp_name))
;       (define pmp_bounds (pmp-decode-napot pmp))

;       (define pmp_start (list-ref pmp_bounds 0))
;       (define pmp_end (bvadd (list-ref pmp_bounds 0) (list-ref pmp_bounds 1)))
      
;       ; test the proper bounds
;       (define slegal (bv-between saddr pmp_start pmp_end))
;       (define elegal (bv-between eaddr pmp_start pmp_end))

;       (when (and slegal elegal)
;         (set! done #t)
;         (if (and (equal? R 1) (equal? W 1) (equal? X 1))
;           (set! legal #t)
;           (set! legal #f)))))
;   legal)

; ; PMP test address ranging from saddr to eaddr 
(define (pmp-check m saddr eaddr)
  #f)
;   ; check pmpcfg0, iterate through each register
;   (define pmpcfg0 (get-csr m 'pmpcfg0))
;   (define pmpcfg0_regs (list 'pmpaddr0 'pmpaddr1 'pmpaddr2 'pmpaddr3
;                              'pmpaddr4 'pmpaddr5 'pmpaddr6 'pmpaddr7))
;   (define legal (pmpcfg-check m pmpcfg0 saddr eaddr pmpcfg0_regs))

;   ; check pmpcfg2, iterate through each register
;   (when (equal? legal null)
;     (define pmpcfg2_regs (list 'pmpaddr8 'pmpaddr9 'pmpaddr10 'pmpaddr11
;                                'pmpaddr12 'pmpaddr13 'pmpaddr14 'pmpaddr15))
;     (define pmpcfg2 (get-csr m 'pmpcfg2))
;     (set! legal (pmpcfg-check m pmpcfg2 saddr eaddr pmpcfg2_regs)))
;   (if (equal? legal null)
;     #t
;     legal))
(provide pmp-check)

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

; TODO: Missing L bit for locking?
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
