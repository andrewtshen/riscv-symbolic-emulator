#lang rosette/safe

(require (only-in racket/base for in-range))
(require syntax/parse/define)
(require (only-in racket/base build-vector))

;; Macros to Build Symbolics

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))

;; CSR Constants
; For more info: https://github.com/d0iasm/rvemu/blob/main/src/csr.rs

(define MXLEN 64)
; The number of CSRs. The field is 12 bits so the maximum kind of CSRs is 4096 (2**12).
(define CSR_SIZE 4096)

#| User-level CSR addresses |#
;; User trap setup.
; User status register.
(define USTATUS #x000)
; User trap handler base address.
(define UTVEC #x005)

;; User trap handling.
; User exception program counter.
(define UEPC #x041)
; User trap cause.
(define UCAUSE #x042)
; User bad address or instruction.
(define UTVAL #x043)

;; User floating-point CSRs.
; Flating-point accrued exceptions.
(define FFLAGS #x001)
; Floating-point dynamic rounding mode.
(define FRB #x002)
; Floating-point control and status register (frm + fflags).
(define FCSR #x003)

;; User Counter/Timers.
; Timer for RDTIME instruction.
(define TIME #xc01)

#| Supervisor-level CSR addresses |#
;; Supervisor trap setup.
; Supervisor status register.
(define SSTATUS #x100)
; Supervisor exception delegation register.
(define SEDELEG #x102)
; Supervisor interrupt delegation register.
(define SIDELEG #x103)
; Supervisor interrupt-enable register.
(define SIE #x104)
; Supervisor trap handler base address.
(define STVEC #x105)

;; Supervisor trap handling.
; Scratch register for supervisor trap handlers.
(define SSCRATCH #x140)
; Supervisor exception program counter.
(define SEPC #x141)
; Supervisor trap cause.
(define SCAUSE #x142)
; Supervisor bad address or instruction.
(define STVAL #x143)
; Supervisor interrupt pending.
(define SIP #x144)

;; Supervisor protection and translation.
; Supervisor address translation and protection.
(define SATP #x180)

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
(define MVENDORID #xf11)
; Architecture ID.
(define MARCHID #xf12)
; Implementation ID.
(define MIMPID #xf13)
; Hardware thread ID.
(define MHARTID #xf14)

;; Machine trap setup.
; Machine status register.
(define MSTATUS #x300)
; ISA and extensions.
(define MISA #x301)
; Machine exception delefation register.
(define MEDELEG #x302)
; Machine interrupt delefation register.
(define MIDELEG #x303)
; Machine interrupt-enable register.
(define MIE #x304)
; Machine trap-handler base address.
(define MTVEC #x305)
; Machine counter enable.
(define MCOUNTEREN #x306)

;; Machine trap handling.
; Scratch register for machine trap handlers.
(define MSCRATCH #x340)
; Machine exception program counter.
(define MEPC #x341)
; Machine trap cause.
(define MCAUSE #x342)
; Machine bad address or instruction.
(define MTVAL #x343)
; Machine interrupt pending.
(define MIP #x344)

;; Machine memory protection.
; Physical memory protection configuration.
(define PMPCFG0 #x3a0)
; Physical memory protection address register.
(define PMPADDR0 #x3b0)

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