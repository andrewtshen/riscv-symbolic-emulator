#lang racket

(require
  "pmp.rkt"
  "fmt.rkt"
  "parameters.rkt")
(require (only-in rosette/safe
                  bv bitvector bveq bvadd bvsub bvand bvor extract concat bitvector->natural))


;; Concrete PMP Check

; PMP test address ranging from saddr to eaddr 
(define (concrete-pmp-check pmp mode saddr eaddr)
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
                  [(equal? mode 1) (set! legal #t)]
                  [(equal? mode 0)
                   ; TODO: actually check what the access type is
                   (set! legal (and (bveq R (bv 1 1)) (bveq W (bv 1 1)) (bveq X (bv 1 1))))]
                  [else
                   ; TODO: implement other mode support
                   (set! legal #f)])
                ; TODO: Implement locked variant of access, for now just return false (no access)
                (set! legal #f))))
        (when (null? legal)
          (set! legal (equal? mode 1)))
        legal)))
(provide concrete-pmp-check)

;; Read/Write from Bytearray
(define (concrete-bytearray-read ba addr nbytes)
  (define bytes
    (for/list ([i (in-range nbytes)])
      ; adjust address for bitvector size (ramsize-log2) and index
      (define adj_addr (extract (sub1 (ramsize-log2)) 0 (bvadd addr (bv i 64))))
      (concrete-memory-read ba adj_addr)))
  ; little endian
  (apply concat (reverse bytes)))
(provide concrete-bytearray-read)

(define (concrete-memory-read mem addr)
  (if (use-fnmem)
      (mem addr)
      (vector-ref mem (bitvector->natural addr))))
(provide concrete-memory-read)
