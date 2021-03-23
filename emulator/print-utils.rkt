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

(define (get-readable-csr csr)
  (cond
    [(bveq csr USTATUS) 'USTATUS]
    [(bveq csr UTVEC) 'UTVEC]
    [(bveq csr UEPC) 'UEPC]
    [(bveq csr UCAUSE) 'UCAUSE]
    [(bveq csr UTVAL) 'UTVAL]
    [(bveq csr FFLAGS) 'FFLAGS]
    [(bveq csr FRB) 'FRB]
    [(bveq csr FCSR ) 'FCSR ]
    [(bveq csr TIME) 'TIME]
    [(bveq csr SSTATUS) 'SSTATUS]
    [(bveq csr SEDELEG) 'SEDELEG]
    [(bveq csr SIDELEG) 'SIDELEG]
    [(bveq csr SIE) 'SIE]
    [(bveq csr STVEC) 'STVEC]
    [(bveq csr SSCRATCH) 'SSCRATCH]
    [(bveq csr SEPC) 'SEPC]
    [(bveq csr SCAUSE) 'SCAUSE]
    [(bveq csr STVAL) 'STVAL]
    [(bveq csr SIP) 'SIP]
    [(bveq csr SATP) 'SATP]
    [(bveq csr SSTATUS_SIE) 'SSTATUS_SIE]
    [(bveq csr SSTATUS_SPIE ) 'SSTATUS_SPIE ]
    [(bveq csr SSTATUS_SPP) 'SSTATUS_SPP]
    [(bveq csr PMPCFG0) 'PMPCFG0]
    [(bveq csr PMPCFG1) 'PMPCFG1]
    [(bveq csr PMPCFG2) 'PMPCFG2]
    [(bveq csr PMPCFG3) 'PMPCFG3]
    [(bveq csr PMPADDR0) 'PMPADDR0]
    [(bveq csr PMPADDR1) 'PMPADDR1]
    [(bveq csr PMPADDR2) 'PMPADDR2]
    [(bveq csr PMPADDR3) 'PMPADDR3]
    [(bveq csr PMPADDR4) 'PMPADDR4]
    [(bveq csr PMPADDR5) 'PMPADDR5]
    [(bveq csr PMPADDR6) 'PMPADDR6]
    [(bveq csr PMPADDR7) 'PMPADDR7]
    [(bveq csr PMPADDR8) 'PMPADDR8]
    [(bveq csr PMPADDR9) 'PMPADDR9]
    [(bveq csr PMPADDR10) 'PMPADDR10]
    [(bveq csr PMPADDR11) 'PMPADDR11]
    [(bveq csr PMPADDR12) 'PMPADDR12]
    [(bveq csr PMPADDR13) 'PMPADDR13]
    [(bveq csr PMPADDR14) 'PMPADDR14]
    [(bveq csr PMPADDR15) 'PMPADDR15]
    [(bveq csr SSTATUS_FS) 'SSTATUS_FS]
    [(bveq csr SSTATUS_XS) 'SSTATUS_XS]
    [(bveq csr SSTATUS_SUM) 'SSTATUS_SUM]
    [(bveq csr SSTATUS_MXR) 'SSTATUS_MXR]
    [(bveq csr SSTATUS_UXL) 'SSTATUS_UXL]
    [(bveq csr MVENDORID) 'MVENDORID]
    [(bveq csr MARCHID) 'MARCHID]
    [(bveq csr MIMPID) 'MIMPID]
    [(bveq csr MHARTID) 'MHARTID]
    [(bveq csr MSTATUS) 'MSTATUS]
    [(bveq csr MISA) 'MISA]
    [(bveq csr MEDELEG) 'MEDELEG]
    [(bveq csr MIDELEG) 'MIDELEG]
    [(bveq csr MIE) 'MIE]
    [(bveq csr MTVEC) 'MTVEC]
    [(bveq csr MCOUNTEREN) 'MCOUNTEREN]
    [(bveq csr MSCRATCH) 'MSCRATCH]
    [(bveq csr MEPC) 'MEPC]
    [(bveq csr MCAUSE) 'MCAUSE]
    [(bveq csr MTVAL) 'MTVAL]
    [(bveq csr MIP) 'MIP]
    [(bveq csr PMPCFG0) 'PMPCFG0]
    [(bveq csr PMPADDR0) 'PMPADDR0]
    [(bveq csr SSIP_BIT) 'SSIP_BIT]
    [(bveq csr MSIP_BIT) 'MSIP_BIT]
    [(bveq csr STIP_BIT) 'STIP_BIT]
    [(bveq csr MTIP_BIT) 'MTIP_BIT]
    [(bveq csr SEIP_BIT) 'SEIP_BIT]
    [(bveq csr MEIP_BIT) 'MEIP_BIT]
    [else
     'UNKNOWN_CSR]))
(provide get-readable-csr)
