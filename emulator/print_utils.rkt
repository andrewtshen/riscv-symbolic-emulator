#lang rosette/safe

(require
  "machine.rkt"
  "pmp.rkt")

(require (only-in racket/base
                  for in-range))

(define (print-pmp m)
  (printf "pmpcfg0:  ~a~n" (machine-csr m 'pmpcfg0))
  (printf "pmpcfg2:  ~a~n" (machine-csr m 'pmpcfg2))
  (printf "pmpaddr0: ~a~n" (machine-csr m 'pmpaddr0))
  (printf "pmpaddr1: ~a~n" (machine-csr m 'pmpaddr1))
  (printf "pmpaddr2: ~a~n" (machine-csr m 'pmpaddr2))
  (printf "pmpaddr3: ~a~n" (machine-csr m 'pmpaddr3))
  (printf "pmpaddr4: ~a~n" (machine-csr m 'pmpaddr4))
  (printf "pmpaddr5: ~a~n" (machine-csr m 'pmpaddr5))
  (printf "pmpaddr6: ~a~n" (machine-csr m 'pmpaddr6))
  (printf "pmpaddr7: ~a~n" (machine-csr m 'pmpaddr7))
  (printf "pmpaddr8: ~a~n" (machine-csr m 'pmpaddr8))
  (printf "pmpaddr0 base/range: ~a~n" (pmp-decode-napot (machine-csr m 'pmpaddr0)))
  (printf "pmpaddr1 base/range: ~a~n" (pmp-decode-napot (machine-csr m 'pmpaddr1)))
  (printf "pmpaddr8 base/range: ~a~n" (pmp-decode-napot (machine-csr m 'pmpaddr8))))
(provide print-pmp)

; ; TODO: fix to work with uf mem or array mem
; (define (print-memory m ramsize)
;   (for [(i (in-range 0 ramsize))]
;     (printf "i: ~a value: ~a~n" i ((machine-ram m) (bv i 32)))))
; (provide print-memory)

; (define (print-memory m start end)
;   (for ([i (in-range start end)])
;     (printf "~x: ~a~n" i (vector-ref (machine-ram m) i))))
; (provide print-memory)

(define (print-csr m)
  (printf "pc: ~a~n" (machine-pc m))
  (printf "mode: ~a~n" (machine-mode m))
  (printf "mtvec: ~a~n" (machine-csr m 'mtvec))
  (printf "mepc: ~a~n" (machine-csr m 'mepc))
  (printf "pmpcfg0: ~a~n" (machine-csr m 'pmpcfg0))
  (printf "pmpcfg2: ~a~n" (machine-csr m 'pmpcfg2))
  (printf "pmpaddr0: ~a~n" (machine-csr m 'pmpaddr0))
  (printf "pmpaddr1: ~a~n" (machine-csr m 'pmpaddr1))
  (printf "pmpaddr2: ~a~n" (machine-csr m 'pmpaddr2))
  (printf "pmpaddr3: ~a~n" (machine-csr m 'pmpaddr3))
  (printf "pmpaddr4: ~a~n" (machine-csr m 'pmpaddr4))
  (printf "pmpaddr5: ~a~n" (machine-csr m 'pmpaddr5))
  (printf "pmpaddr6: ~a~n" (machine-csr m 'pmpaddr6))
  (printf "pmpaddr7: ~a~n" (machine-csr m 'pmpaddr7))
  (printf "pmpaddr8: ~a~n" (machine-csr m 'pmpaddr8)))
(provide print-csr)
