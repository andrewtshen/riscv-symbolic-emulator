#lang rosette/safe

(require
  "init.rkt"
  "emulate.rkt"
  "machine.rkt"
  "pmp.rkt"
  "execute.rkt"
  "parameters.rkt"
  "print-utils.rkt")
(require (only-in racket/base 
  parameterize for in-range for* define-syntax-rule symbol?))
(require profile)

; (printf "* Running boot sequence test ~n")
; (define program (file->bytearray "kernel/kernel.bin"))
; (define m
;  (time (parameterize
;       ([use-sym-optimizations #f]
;        [use-debug-mode #f]
;        [use-fnmem #f]
;        [use-concrete-optimizations #t])
;     (init-machine-with-prog program))))
; (profile-thunk
;  (lambda ()
;     (parameterize
;      ([use-sym-optimizations #f]
;       [use-debug-mode #f]
;       [use-fnmem #f]
;       [use-concrete-optimizations #t])
;     (execute-until-mret m))))
; ; (time (parameterize
; ;        ([use-sym-optimizations #f]
; ;         [use-debug-mode #f]
; ;         [use-fnmem #f]
; ;         [use-concrete-optimizations #t])
; ;       (execute-until-mret m)))

; ; Check that after boot sequence machine mode is user mode (0) and in OK state
; (print-pmp m)

(define program (file->bytearray "build/addiw.bin"))
(printf "* Running addiw.bin test ~n")
(define m
 (parameterize
     ([use-debug-mode #f]
      [debug-instr #t]
      [use-fnmem #f])
   (init-machine-with-prog program)))
(printf "RES: ~a~n" (parameterize
   ([use-debug-mode #f]
    [debug-instr #t]
    [use-fnmem #f])
 (execute-until-mret m)))
