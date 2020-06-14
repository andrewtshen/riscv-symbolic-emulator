#lang rosette/safe

(require (only-in racket/base make-parameter))

;; Default setting for parameters

; Use the symbolic optimizations for verifying the kernel
(define use-sym-optimizations (make-parameter #f))
(provide use-sym-optimizations)

; Use debugging statements
(define use-debug-mode (make-parameter #f))
(provide use-debug-mode)

; RAM size log2
(define ramsize-log2 (make-parameter 24))
(provide ramsize-log2)

(define use-fnmem (make-parameter #t))
(provide use-fnmem)

; Base address
; (base address is in 64 because though the pointer to memory is 32
; we are using RISC-V 64)
(define base-address (make-parameter (bv #x80000000 64)))
(provide base-address)

