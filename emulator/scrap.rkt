#lang rosette/safe

(require
  "init.rkt"
  "emulate.rkt"
  "execute.rkt"
  "machine.rkt"
  "pmp.rkt"
  "decode.rkt"
  "parameters.rkt"
  "print_utils.rkt")
(require (only-in racket/base 
  parameterize for in-range for*))
(require profile)

(define program (file->bytearray "kernel/kernel.bin"))
(define m
  (time (parameterize
          ([use-sym-optimizations #f]
           [use-debug-mode #f]
           [use-fnmem #f]
           [use-concrete-mem #f]
           [ramsize-log2 20])
        (init-machine-with-prog program))))

(time (parameterize
        ([use-sym-optimizations #f]
         [use-debug-mode #f]
         [use-fnmem #f]
         [use-concrete-mem #f])
      (execute-until-mret m)))
