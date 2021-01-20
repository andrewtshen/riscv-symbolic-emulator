#lang rosette/safe

(require
  "machine.rkt"
  "pmp.rkt")

(require (only-in racket/base
                  for in-range))

(define (print-pmp m)
  (printf "pmpcfg0: ~a~n" (get-csr m 'pmpcfg0))
  (printf "pmpcfg2: ~a~n" (get-csr m 'pmpcfg2))
  (printf "pmpaddr0: ~a~n" (get-csr m 'pmpaddr0))
  (printf "pmpaddr1: ~a~n" (get-csr m 'pmpaddr1))
  (printf "pmpaddr2: ~a~n" (get-csr m 'pmpaddr2))
  (printf "pmpaddr3: ~a~n" (get-csr m 'pmpaddr3))
  (printf "pmpaddr4: ~a~n" (get-csr m 'pmpaddr4))
  (printf "pmpaddr5: ~a~n" (get-csr m 'pmpaddr5))
  (printf "pmpaddr6: ~a~n" (get-csr m 'pmpaddr6))
  (printf "pmpaddr7: ~a~n" (get-csr m 'pmpaddr7))
  (printf "pmpaddr8: ~a~n" (get-csr m 'pmpaddr8))
  (printf "pmpaddr0 base/range: ~a~n" (pmp-decode-napot (get-csr m 'pmpaddr0)))
  (printf "pmpaddr1 base/range: ~a~n" (pmp-decode-napot (get-csr m 'pmpaddr1)))
  (printf "pmpaddr8 base/range: ~a~n" (pmp-decode-napot (get-csr m 'pmpaddr8))))
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
  (printf "pc: ~a~n" (get-pc m))
  (printf "mode: ~a~n" (machine-mode m))
  (printf "mtvec: ~a~n" (get-csr m 'mtvec))
  (printf "mepc: ~a~n" (get-csr m 'mepc))
  (printf "pmpcfg0: ~a~n" (get-csr m 'pmpcfg0))
  (printf "pmpcfg2: ~a~n" (get-csr m 'pmpcfg2))
  (printf "pmpaddr0: ~a~n" (get-csr m 'pmpaddr0))
  (printf "pmpaddr1: ~a~n" (get-csr m 'pmpaddr1))
  (printf "pmpaddr2: ~a~n" (get-csr m 'pmpaddr2))
  (printf "pmpaddr3: ~a~n" (get-csr m 'pmpaddr3))
  (printf "pmpaddr4: ~a~n" (get-csr m 'pmpaddr4))
  (printf "pmpaddr5: ~a~n" (get-csr m 'pmpaddr5))
  (printf "pmpaddr6: ~a~n" (get-csr m 'pmpaddr6))
  (printf "pmpaddr7: ~a~n" (get-csr m 'pmpaddr7))
  (printf "pmpaddr8: ~a~n" (get-csr m 'pmpaddr8)))
(provide print-csr)