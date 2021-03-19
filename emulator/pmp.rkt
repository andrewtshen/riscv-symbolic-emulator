#lang rosette/safe

(require (only-in racket/base for in-range))
(require syntax/parse/define)
(require (only-in racket/base build-vector))

;; Macros to Build PMP

(define-simple-macro (fresh-symbolic name type)
                     (let () (define-symbolic* name type) name))

(define-simple-macro (make-pmpaddrs n:expr)
                     (build-vector n (lambda (i) (define p (make-pmpaddr)) p)))

(define-simple-macro (make-pmpcfgs n:expr)
                     (build-vector n (lambda (i) (define p (make-pmpcfg)) p)))

;; Structs to Build PMP

(struct pmp
  (pmpcfgs pmpaddrs numimplemented)
  #:mutable #:transparent)
(provide (struct-out pmp))

; pmpcfg struct
(struct pmpcfg
  (value settings)
  #:mutable #:transparent)
(provide (struct-out pmpcfg))

; pmpaddr struct
(struct pmpaddr
  (value startaddr endaddr)
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

(define (make-pmp)
  (define numimplemented 0)
  (pmp
    (make-pmpcfgs 2)
    (make-pmpaddrs 16)
    numimplemented))
(provide make-pmp)

(define (make-pmpcfg-setting)
  (pmpcfg-setting
    (bv 0 1)
    (bv 0 1)
    (bv 0 1)
    (bv 0 2)
    (bv 0 1)))

(define (make-pmpcfg)
  (pmpcfg
    (bv 0 64)
    (make-pmpcfg-settings 8)))

(define (make-pmpaddr)
  (pmpaddr
    (bv 0 64)
    (bv 0 64)
    (bv 0 64)))

;; PMP utilities for decoding registers and checking

(define (get-pmpicfg-setting pmpcfg i)
  (vector-ref (pmpcfg-settings pmpcfg) i))
(provide get-pmpicfg-setting)

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
  (pmpcfg-setting R W X A L))
(provide pmp-decode-cfg)

; Check if pmp-setting is implemented
(define (pmp-is-implemented? pmp-setting)
  (bveq (pmpcfg-setting-A pmp-setting) (bv 0 2)))
(provide pmp-is-implemented?)

; Check if pmp-setting is locked
(define (pmp-is-locked? pmp-setting)
  (bveq (pmpcfg-setting-L pmp-setting) (bv 1 1)))
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
  (equal? (pmp-numimplemented pmp) 0))
(provide pmp-none-impl?)

(define (pmp-pmpaddri pmp i)
  (vector-ref (pmp-pmpaddrs pmp) i))
(provide pmp-pmpaddri)

(define (pmp-pmpcfgi pmp i)
  (vector-ref (pmp-pmpcfgs pmp) i))
(provide pmp-pmpcfgi)

(define (get-pmpaddri-setting pmp i)
  (let
    ([pmpcfg0 (pmp-pmpcfgi pmp 0)]
     [pmpcfg2 (pmp-pmpcfgi pmp 1)])
    ; Iterate through each pmpaddr and break at first matching
    (if (< i 8)
        (get-pmpicfg-setting pmpcfg0 i)
        (get-pmpicfg-setting pmpcfg2 (- i 8)))))

(define (set-pmpaddri! pmp i val)
  (define setting (get-pmpaddri-setting pmp i))
  
  (when (not (pmp-is-locked? setting))
    ; Set the value for the pmp first
    (set-pmpaddr-value! (pmp-pmpaddri pmp i) val)
    
    ; Decode the value
    (define pmp_bounds (pmp-decode-napot val))
    (define pmp_start (list-ref pmp_bounds 0))
    (define pmp_end (bvadd (list-ref pmp_bounds 0) (list-ref pmp_bounds 1)))
    
    (set-pmpaddr-startaddr! (pmp-pmpaddri pmp i) pmp_start)
    (set-pmpaddr-endaddr! (pmp-pmpaddri pmp i) pmp_end)))
(provide set-pmpaddri!)

(define (set-pmpcfgi! pmp i val)
  ; (set-pmpcfg-value! (pmp-pmpcfgi pmp i) val)
  (define pmpcfgi (pmp-pmpcfgi pmp i))
  (for ([id (in-range 8)])
    (define old_settings (get-pmpicfg-setting pmpcfgi id))
    
    (when (not (pmp-is-locked? old_settings))
      (define new_settings (pmp-decode-cfg val id))
      
      ; Adjust Number of Implemented PMPs
      (when (and (pmp-is-implemented? old_settings)
                 (not (pmp-is-implemented? new_settings)))
        (set-pmp-numimplemented! pmp (add1 (pmp-numimplemented pmp))))
      
      (when (and (not (pmp-is-implemented? old_settings))
                 (pmp-is-implemented? new_settings))
        (set-pmp-numimplemented! pmp (sub1 (pmp-numimplemented pmp))))
      
      ; Update pmpcfg value for pmp(id)cfg(i)
      (define start (* id 8))
      (define end (* (add1 id) 8))
      (define original_value (pmpcfg-value pmpcfgi))
      
      (set-pmpcfg-value!
        pmpcfgi
        ; Determine new value based on which part of the bitvector we are modifying
        ; since the extraction is a little bit weird (can't extract size 0)
        (cond 
          [(equal? start 0)
           (concat
             (extract 63 end original_value)
             (extract (sub1 end) start val))]
          [(equal? end 64)
           (concat
             (extract (sub1 end) start val)
             (extract (sub1 start) 0 original_value))]
          [else
           (concat
             (extract 63 end original_value)
             (extract (sub1 end) start val)
             (extract (sub1 start) 0 original_value))]))
      
      ; Update settings
      (vector-set! (pmpcfg-settings pmpcfgi) id new_settings))))
(provide set-pmpcfgi!)

; PMP test address ranging from saddr to eaddr 
(define (pmp-check pmp mode saddr eaddr)
  (if (pmp-none-impl? pmp) #t
      (let
        ([legal null]
         [pmpcfg0 (pmp-pmpcfgi pmp 0)]
         [pmpcfg2 (pmp-pmpcfgi pmp 1)])
        ; Iterate through each pmpaddr and break at first matching
        (for ([i (in-range 16)])
          #:break (not (equal? legal null))
          (define setting (if (< i 8)
                              (get-pmpicfg-setting pmpcfg0 i)
                              (get-pmpicfg-setting pmpcfg2 (- i 8))))
          (define R (pmpcfg-setting-R setting))
          (define W (pmpcfg-setting-W setting))
          (define X (pmpcfg-setting-X setting))
          (define A (pmpcfg-setting-A setting))
          (define L (pmpcfg-setting-L setting))
          ; For now we only implement A = 3 (NAPOT)
          (define bounds 
            (cond
              [(bveq A (bv 3 2))
               (let*
                 ([pmpaddr (pmp-pmpaddri pmp i)]
                  [pmp_start (pmpaddr-startaddr pmpaddr)]
                  [pmp_end (pmpaddr-endaddr pmpaddr)]
                  ; Test the proper bounds, #t means allow access, #f means disallow access
                  [slegal (bv-between saddr pmp_start pmp_end)]
                  [elegal (bv-between eaddr pmp_start pmp_end)])
                 (list slegal elegal))]
              [else
               (list #f #f)]))
          
          (define slegal (list-ref bounds 0))    
          (define elegal (list-ref bounds 1)) 
          ; Check saddr and eaddr match the pmpaddri range
          (when (and slegal elegal)
            ; Check if pmpaddri is locked
            (if (not (pmp-is-locked? setting))
                ; Check machine mode
                (cond
                  [(bveq mode (bv 1 3)) (set! legal #t)]
                  [(bveq mode (bv 0 3))
                   ; TODO: actually check what the access type is
                   (set! legal (and (bveq R (bv 1 1)) (bveq W (bv 1 1)) (bveq X (bv 1 1))))]
                  [else
                   ; TODO: implement other mode support
                   (set! legal #f)])
                ; TODO: Implement locked variant of access, for now just return false (no access)
                (set! legal #f))))
        (when (null? legal)
          (set! legal (bveq mode (bv 1 3))))
        legal)))
(provide pmp-check)

; Check if bv1 satisfies bv2 <= bv1 <= bv3
(define (bv-between bv1 bv2 bv3)
  (and (bvule bv2 bv1) (bvule bv1 bv3)))
(provide bv-between)

; (printf "base: #x80000000, size: #x20000 ~a~n" (pmp-encode-napot (bv #x80020000 64) (bv #x20000 64)))
; (printf "decoding #x: ~a~n" (pmp-decode-napot (bv #x7fffffffffffffff 64)))
; (printf "decoding #x: ~a~n" (pmp-decode-napot (bv #x00000000200003ff 64)))
