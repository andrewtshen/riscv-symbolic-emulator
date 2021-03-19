#lang rosette/safe

(require
  "machine.rkt"
  "pmp.rkt"
  "csrs.rkt")

(require (only-in racket/base
                  for in-range))

(define (print-pmp m)
  (printf "pmpcfg0:  ~a~n" (machine-csr m PMPCFG0))
  (printf "pmpcfg2:  ~a~n" (machine-csr m PMPCFG2))
  (printf "pmpaddr0: ~a~n" (machine-csr m PMPADDR0))
  (printf "pmpaddr1: ~a~n" (machine-csr m PMPADDR1))
  (printf "pmpaddr2: ~a~n" (machine-csr m PMPADDR2))
  (printf "pmpaddr3: ~a~n" (machine-csr m PMPADDR3))
  (printf "pmpaddr4: ~a~n" (machine-csr m PMPADDR4))
  (printf "pmpaddr5: ~a~n" (machine-csr m PMPADDR5))
  (printf "pmpaddr6: ~a~n" (machine-csr m PMPADDR6))
  (printf "pmpaddr7: ~a~n" (machine-csr m PMPADDR7))
  (printf "pmpaddr8: ~a~n" (machine-csr m PMPADDR8))
  (printf "pmpaddr0 base/range: ~a~n" (pmp-decode-napot (machine-csr m PMPADDR0)))
  (printf "pmpaddr1 base/range: ~a~n" (pmp-decode-napot (machine-csr m PMPADDR1)))
  (printf "pmpaddr8 base/range: ~a~n" (pmp-decode-napot (machine-csr m PMPADDR8))))
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
  (printf "mtvec: ~a~n" (machine-csr m MTVEC))
  (printf "mepc: ~a~n" (machine-csr m MEPC))
  (printf "pmpcfg0: ~a~n" (machine-csr m PMPCFG0))
  (printf "pmpcfg2: ~a~n" (machine-csr m PMPCFG2))
  (printf "pmpaddr0: ~a~n" (machine-csr m PMPADDR0))
  (printf "pmpaddr1: ~a~n" (machine-csr m PMPADDR1))
  (printf "pmpaddr2: ~a~n" (machine-csr m PMPADDR2))
  (printf "pmpaddr3: ~a~n" (machine-csr m PMPADDR3))
  (printf "pmpaddr4: ~a~n" (machine-csr m PMPADDR4))
  (printf "pmpaddr5: ~a~n" (machine-csr m PMPADDR5))
  (printf "pmpaddr6: ~a~n" (machine-csr m PMPADDR6))
  (printf "pmpaddr7: ~a~n" (machine-csr m PMPADDR7))
  (printf "pmpaddr8: ~a~n" (machine-csr m PMPADDR8)))
(provide print-csr)
