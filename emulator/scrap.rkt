#lang rosette/safe

(require
  "init.rkt"
  "emulate.rkt"
  "execute.rkt"
  "machine.rkt"
  "pmp.rkt"
  "decode.rkt"
  "parameters.rkt"
  "print-utils.rkt")
(require (only-in racket/base 
  parameterize for in-range for*))
(require profile)

(printf "* Running boot sequence test ~n")
(define program (file->bytearray "kernel/kernel.bin"))
(define m
 (time (parameterize
      ([use-sym-optimizations #f]
       [use-debug-mode #f]
       [use-fnmem #f]
       [use-concrete-optimizations #t])
    (init-machine-with-prog program))))
(profile-thunk
 (lambda ()
    (parameterize
     ([use-sym-optimizations #f]
      [use-debug-mode #f]
      [use-fnmem #f]
      [use-concrete-optimizations #t])
    (execute-until-mret m))))
; (time (parameterize
;        ([use-sym-optimizations #f]
;         [use-debug-mode #f]
;         [use-fnmem #f]
;         [use-concrete-optimizations #t])
;       (execute-until-mret m)))

; Check that after boot sequence machine mode is user mode (0) and in OK state
(print-pmp m)
