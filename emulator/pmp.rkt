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

; Check if pmp_setting is implemented
(define (pmp-is-implemented? pmp_setting)
  (bveq (pmpcfg_setting-A pmp_setting) (bv 0 2)))
(provide pmp-is-implemented?)

; Check if pmp_setting is locked
(define (pmp-is-locked? pmp_setting)
  (bveq (pmpcfg_setting-L pmp_setting) (bv 1 1)))
(provide pmp-is-locked?)

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

(define (pmp-none-impl? pmp)
  (equal? (pmp-num_implemented pmp) 0))
(provide pmp-none-impl?)

(define (pmp-pmpaddri pmp i)
  (vector-ref (pmp-pmpaddrs pmp) i))
(provide pmp-pmpaddri)

(define (pmp-pmpcfgi pmp i)
  (vector-ref (pmp-pmpcfgs pmp) i))
(provide pmp-pmpcfgi)

; REWRITE PMP test address ranging from saddr to eaddr 
(define (test-pmp-check pmp mode saddr eaddr)
  (define legal #t)
  (if (pmp-none-impl? pmp) legal
    (let
      ([legal null]
       [pmpcfg0 (pmp-pmpcfgi pmp 0)]
       [pmpcfg2 (pmp-pmpcfgi pmp 1)])
      ; Iterate through each pmpaddr and break at first matching
      (for ([i (in-range 16)])
        #:break (not (equal? legal null))
        (let*
          ([setting (if (< i 8)
                      (get-pmpcfg-setting pmpcfg0 i)
                      (get-pmpcfg-setting pmpcfg2 (- i 8)))]
           [R (pmpcfg_setting-R setting)]
           [W (pmpcfg_setting-W setting)]
           [X (pmpcfg_setting-X setting)]
           [A (pmpcfg_setting-A setting)]
           [L (pmpcfg_setting-L setting)])
          ; For now we only implement A = 3 (NAPOT)
          (define bounds 
            (cond
              [(bveq A (bv 0 2))
                 ; Unimplemented, so just return no access
                 (list #f #f)]
              [(bveq A (bv 3 2))
                (let*
                  ([pmpaddr (pmp-pmpaddri pmp i)]
                   [pmp_start (pmpaddr-start_addr pmpaddr)]
                   [pmp_end (pmpaddr-end_addr pmpaddr)]
                   ; Test the proper bounds, #t means allow access, #f means disallow access
                   [slegal (bv-between saddr pmp_start pmp_end)]
                   [elegal (bv-between eaddr pmp_start pmp_end)])
                  (list slegal elegal))]
              [else
                (list #f #f)]))

         (define slegal (list-ref bounds 0))    
         (define elegal (list-ref bounds 1)) 
         ; Check saddr and eaddr match the pmpaddri range
         (if (and slegal elegal)
             ; Check if pmpaddri is locked
             (if (not (pmp-is-locked? setting))
                 ; Check machine mode
                 (cond
                   [(equal? mode 1) (set! legal #t)]
                   [(equal? mode 0)
                    ; TODO: actually check what the access type is
                    (set! legal (and (bveq R (bv 1 1)) (bveq W (bv 1 1)) (bveq X (bv 1 1))))]
                   [else
                    ; TODO: implement other mode support
                    (set! legal #f)])
                 ; TODO: Implement locked variant of access, for now just return false (no access)
                 (set! legal #f))
             ; from earlier checks there must have been at least 1 pmpaddr active
             (when (equal? i 15) (set! legal #f)))))
      legal)))
(provide test-pmp-check)

; Check if bv1 satisfies bv2 <= bv1 <= bv3
(define (bv-between bv1 bv2 bv3)
  (and (bvule bv2 bv1) (bvule bv1 bv3)))
(provide bv-between)

; (printf "base: #x80000000, size: #x20000 ~a~n" (pmp-encode-napot (bv #x80020000 64) (bv #x20000 64)))
; (printf "decoding #x: ~a~n" (pmp-decode-napot (bv #x7fffffffffffffff 64)))
; (printf "decoding #x: ~a~n" (pmp-decode-napot (bv #x00000000200003ff 64)))
