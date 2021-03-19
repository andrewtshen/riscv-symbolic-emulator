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
                  custodian-limit-memory current-custodian parameterize call-with-parameterization
                  parameterize* for for/list for/vector in-range))
(require rackunit rackunit/text-ui)
(require profile)

;; RISC-V Tests for Individual Instructions

(define-test-suite riscv-tests
  (test-case "add test"
             (printf "* riscv-tests/isa/rv64ui-p-add.bin ~n")
             (define program (file->bytearray "riscv-tests/isa/rv64ui-p-add.bin"))
             (define m (parameterize
                           ([use-debug-mode #f]
                            [use-fnmem #f])
                         (init-machine-with-prog program)))
             (parameterize
                 ([use-debug-mode #f]
                  [use-fnmem #f])
               (execute-until-mret m))))

(define res-instruction-check (run-tests riscv-tests))
