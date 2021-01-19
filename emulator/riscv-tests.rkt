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
  custodian-limit-memory current-custodian parameterize call-with-parameterization parameterize* for for/list for/vector in-range))
(require rackunit rackunit/text-ui)
(require profile)

;; RISC-V Tests for Individual Instructions

(define-test-suite instruction-check
  ; (test-case "own add test"
  ;   (define program (file->bytearray "build/add.bin"))
  ;   (printf "* Running add.bin test ~n")
  ;   (define m (parameterize
  ;     ([use-debug-mode #f])
  ;     (init-machine-with-prog program)))
  ;   (parameterize
  ;     ([use-debug-mode #f])
  ;     (execute-until-mret m)))
  (test-case "add test"
    (define program (file->bytearray "riscv-tests-build/build/rv64ui-p-add.bin"))
    (printf "* Running add.bin test ~n")
    (define m (parameterize
      ([use-debug-mode #f]
       [use-fnmem #f])
      (init-machine-with-prog program)))
    (parameterize
      ([use-debug-mode #f]
       [use-fnmem #f])
      (execute-until-mret m))
    ; (define gprsx
    ;   (for/list ([i (in-range 10 18)])
    ;     (gprs-get-x m i)))
    ; (define model_add (verify (begin 
    ;   (assert (bveq (bvadd (list-ref gprsx 5) (list-ref gprsx 7))
    ;                 (list-ref gprsx 6))))))
    ; (check-true (unsat? model_add))
    ))

(define res-instruction-check (run-tests instruction-check))
