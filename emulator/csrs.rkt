#lang rosette/safe

(require
  "parameters.rkt")
(require syntax/parse/define)
(require (only-in racket/base
                  build-vector for in-range))

;; Macros to Build Symbolics

; Create a vector of symbolic variables
(define-simple-macro (make-sym-vector n:expr size:expr m:id)
  (if (use-concrete-mem)
    (build-vector n (lambda (i) (bv 0 size)))
    (build-vector n (lambda (i) (define-symbolic* m (bitvector size)) m))))

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))


;; Mode Constants
(provide
  U_MODE S_MODE M_MODE
  U_MODE? S_MODE? M_MODE?)

(define U_MODE (bv #b00 2))
(define S_MODE (bv #b01 2))
(define M_MODE (bv #b11 2))

(define (U_MODE? mode)
  (bveq mode U_MODE))

(define (S_MODE? mode)
  (bveq mode S_MODE))

(define (M_MODE? mode)
  (bveq mode M_MODE))


;; CSR Constants
; For more info: https://github.com/d0iasm/rvemu/blob/main/src/csr.rs

(provide MXLEN CSR_SIZE
         USTATUS UTVEC UEPC UCAUSE UTVAL 
         FFLAGS FRB FCSR TIME
         SSTATUS SEDELEG SIDELEG SIE STVEC SSCRATCH SEPC SCAUSE STVAL
         SIP SATP SSTATUS_SIE SSTATUS_SPIE SSTATUS_SPP
         PMPCFG0 PMPCFG1 PMPCFG2 PMPCFG3 PMPADDR0 PMPADDR1 PMPADDR2 PMPADDR3
         PMPADDR4 PMPADDR5 PMPADDR6 PMPADDR7 PMPADDR8 PMPADDR9 PMPADDR10
         PMPADDR11 PMPADDR12 PMPADDR13 PMPADDR14 PMPADDR15 
         SSTATUS_FS SSTATUS_XS SSTATUS_SUM SSTATUS_MXR SSTATUS_UXL
         MVENDORID MARCHID MIMPID MHARTID MSTATUS MISA MEDELEG MIDELEG MIE 
         MTVEC MCOUNTEREN MSCRATCH MEPC MCAUSE MTVAL MIP PMPCFG0 PMPADDR0
         SSIP_BIT MSIP_BIT STIP_BIT MTIP_BIT SEIP_BIT MEIP_BIT)
; Commented out: SSTATUS_UIE SSTATUS_UPIE

; RV64
(define MXLEN 64)

; The number of CSRs. The field is 12 bits so the maximum kind of CSRs is 4096 (2**12).
(define CSR_SIZE 4096)


#| User-level CSR addresses |#
;; User trap setup.
; User status register.
(define USTATUS (bv #x000 12))
; User trap handler base address.
(define UTVEC (bv #x005 12))

;; User trap handling.
; User exception program counter.
(define UEPC (bv #x041 12))
; User trap cause.
(define UCAUSE (bv #x042 12))
; User bad address or instruction.
(define UTVAL (bv #x043 12))

;; User floating-point CSRs.
; Flating-point accrued exceptions.
(define FFLAGS (bv #x001 12))
; Floating-point dynamic rounding mode.
(define FRB (bv #x002 12))
; Floating-point control and status register (frm + fflags).
(define FCSR (bv #x003 12))

;; User Counter/Timers.
; Timer for RDTIME instruction.
(define TIME (bv #xc01 12))

#| Supervisor-level CSR addresses |#
;; Supervisor trap setup.
; Supervisor status register.
(define SSTATUS (bv #x100 12))
; Supervisor exception delegation register.
(define SEDELEG (bv #x102 12))
; Supervisor interrupt delegation register.
(define SIDELEG (bv #x103 12))
; Supervisor interrupt-enable register.
(define SIE (bv #x104 12))
; Supervisor trap handler base address.
(define STVEC (bv #x105 12))

;; Supervisor trap handling.
; Scratch register for supervisor trap handlers.
(define SSCRATCH (bv #x140 12))
; Supervisor exception program counter.
(define SEPC (bv #x141 12))
; Supervisor trap cause.
(define SCAUSE (bv #x142 12))
; Supervisor bad address or instruction.
(define STVAL (bv #x143 12))
; Supervisor interrupt pending.
(define SIP (bv #x144 12))

;; Supervisor protection and translation.
; Supervisor address translation and protection.
(define SATP (bv #x180 12))

;; SSTATUS fields.
;(define SSTATUS_UIE (bv #x00000001 64)
(define SSTATUS_SIE (bv #x00000002 64))
;(define SSTATUS_UPIE (bv #x00000010 64)
(define SSTATUS_SPIE (bv #x00000020 64))
(define SSTATUS_SPP (bv #x00000100 64))
(define SSTATUS_FS (bv #x00006000 64))
(define SSTATUS_XS (bv #x00018000 64))
(define SSTATUS_SUM (bv #x00040000 64))
(define SSTATUS_MXR (bv #x00080000 64))
(define SSTATUS_UXL (bv #x0000000300000000 64))

#| Machine-level CSR addresses |#
;; Machine information registers.
; Vendor ID.
(define MVENDORID (bv #xf11 12))
; Architecture ID.
(define MARCHID (bv #xf12 12))
; Implementation ID.
(define MIMPID (bv #xf13 12))
; Hardware thread ID.
(define MHARTID (bv #xf14 12))

;; Machine trap setup.
; Machine status register.
(define MSTATUS (bv #x300 12))
; ISA and extensions.
(define MISA (bv #x301 12))
; Machine exception delefation register.
(define MEDELEG (bv #x302 12))
; Machine interrupt delefation register.
(define MIDELEG (bv #x303 12))
; Machine interrupt-enable register.
(define MIE (bv #x304 12))
; Machine trap-handler base address.
(define MTVEC (bv #x305 12))
; Machine counter enable.
(define MCOUNTEREN (bv #x306 12))

;; Machine trap handling.
; Scratch register for machine trap handlers.
(define MSCRATCH (bv #x340 12))
; Machine exception program counter.
(define MEPC (bv #x341 12))
; Machine trap cause.
(define MCAUSE (bv #x342 12))
; Machine bad address or instruction.
(define MTVAL (bv #x343 12))
; Machine interrupt pending.
(define MIP (bv #x344 12))

;; Machine memory protection.
; Physical memory protection configuration.
(define PMPCFG0 (bv #x3a0 12))
(define PMPCFG1 (bv #x3a1 12))
(define PMPCFG2 (bv #x3a2 12))
(define PMPCFG3 (bv #x3a3 12))

; Physical memory protection address register.
(define PMPADDR0 (bv #x3b0 12))
(define PMPADDR1 (bv #x3b1 12))
(define PMPADDR2 (bv #x3b2 12))
(define PMPADDR3 (bv #x3b3 12))
(define PMPADDR4 (bv #x3b4 12))
(define PMPADDR5 (bv #x3b5 12))
(define PMPADDR6 (bv #x3b6 12))
(define PMPADDR7 (bv #x3b7 12))
(define PMPADDR8 (bv #x3b8 12))
(define PMPADDR9 (bv #x3b9 12))
(define PMPADDR10 (bv #x3bA 12))
(define PMPADDR11 (bv #x3bb 12))
(define PMPADDR12 (bv #x3bc 12))
(define PMPADDR13 (bv #x3bd 12))
(define PMPADDR14 (bv #x3be 12))
(define PMPADDR15 (bv #x3bf 12))

;; MIP fields.
; Supervisor software interrupt.
(define SSIP_BIT (bvshl (bv 1 64) (bv 1 64)))
; Machine software interrupt.
(define MSIP_BIT (bvshl (bv 1 64) (bv 3 64)))
; Supervisor timer interrupt.
(define STIP_BIT (bvshl (bv 1 64) (bv 5 64)))
; Machine timer interrupt.
(define MTIP_BIT (bvshl (bv 1 64) (bv 7 64)))
; Supervisor external interrupt.
(define SEIP_BIT (bvshl (bv 1 64) (bv 9 64)))
; Machine external interrupt.
(define MEIP_BIT (bvshl (bv 1 64) (bv 11 64)))

;; CSRS Methods

(define (make-csrs)
  (make-sym-vector CSR_SIZE MXLEN csrs))
(provide make-csrs)

(define (get-csr csrs csr)
  (vector-ref-bv csrs csr))
(provide get-csr)

(define (set-csr! csrs csr val)
  (vector-set!-bv csrs csr val))
(provide set-csr!)

(define (csr-access? csr mode)
  (bvule (extract 9 8 csr) mode))
(provide csr-access?)

; (define csrs (make-sym-vector CSR_SIZE MXLEN csrs))
; (printf "~a~n" (get-csr csrs PMPCFG0))
; (set-csr! csrs PMPCFG0 (bv 1 64))
; (printf "~a~n" (get-csr csrs PMPCFG0))
