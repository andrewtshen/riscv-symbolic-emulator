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

(define (print-test-error a0)
  (printf "Errored on test: ~a~n" (bvlshr a0 (bv 1 64))))

(define (execute-test testname)
  (printf "* ~a ~n" testname)
  (define program (file->bytearray testname))
  (define m (parameterize
               ([use-debug-mode #f]
                [use-fnmem #f])
             (init-machine-with-prog program)))
  (define last_instr 
    (parameterize
       ([use-debug-mode #f]
        [use-fnmem #f])
     (execute-until-ecall m)))
  (define a0 (get-gprs-i (machine-gprs m) (bv 10 5)))
  (check-equal? last_instr '(ecall))
  (when (not (bveq a0 (bv 0 64)))
    (print-test-error a0))
  (check-equal? a0 (bv 0 64)))

(define-test-suite riscv-tests
  ; (test-case "add"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-add.bin"))
  ; (test-case "addi"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-addi.bin"))
  ; (test-case "addiw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-addiw.bin"))
  ; (test-case "addw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-addw.bin"))
  ; (test-case "and"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-and.bin"))
  ; (test-case "andi"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-andi.bin"))
  ; (test-case "auipc"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-auipc.bin"))
  ; (test-case "beq"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-beq.bin"))
  ; (test-case "bge"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-bge.bin"))
  ; (test-case "bgeu"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-bgeu.bin"))
  ; (test-case "blt"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-blt.bin"))
  ; (test-case "bltu"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-bltu.bin"))
  ; (test-case "bne"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-bne.bin"))
  ; TODO: Find out how fence_i works
  ; (test-case "fence_i"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-fence_i.bin"))
  ; (test-case "jal"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-jal.bin"))
  ; (test-case "jalr"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-jalr.bin"))
  ; (test-case "lb"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-lb.bin"))
  ; (test-case "lbu"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-lbu.bin"))
  ; (test-case "ld"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-ld.bin"))
  ; (test-case "lh"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-lh.bin"))
  ; (test-case "lhu"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-lhu.bin"))
  ; (test-case "lui"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-lui.bin"))
  ; (test-case "lw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-lw.bin"))
  ; (test-case "lwu"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-lwu.bin"))
  ; (test-case "or"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-or.bin"))
  ; (test-case "ori"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-ori.bin"))
  ; (test-case "sb"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sb.bin"))
  ; (test-case "sd"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sd.bin"))
  ; (test-case "sh"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sh.bin"))
  (test-case "simple"
             (execute-test "riscv-tests/isa/rv64ui-p-simple.bin"))
  (test-case "sll"
             (execute-test "riscv-tests/isa/rv64ui-p-sll.bin")))
  ; (test-case "slli"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-slli.bin"))
  ; (test-case "slliw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-slliw.bin"))
  ; (test-case "sllw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sllw.bin"))
  ; (test-case "slt"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-slt.bin"))
  ; (test-case "slti"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-slti.bin"))
  ; (test-case "sltiu"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sltiu.bin"))
  ; (test-case "sltu"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sltu.bin"))
  ; (test-case "sra"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sra.bin"))
  ; (test-case "srai"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-srai.bin"))
  ; (test-case "sraiw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sraiw.bin"))
  ; (test-case "sraw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sraw.bin"))
  ; (test-case "srl"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-srl.bin"))
  ; (test-case "srli"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-srli.bin"))
  ; (test-case "srliw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-srliw.bin"))
  ; (test-case "srlw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-srlw.bin"))
  ; (test-case "sub"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sub.bin"))
  ; (test-case "subw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-subw.bin"))
  ; (test-case "sw"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-sw.bin"))
  ; (test-case "xor"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-xor.bin"))
  ; (test-case "xori"
  ;            (execute-test "riscv-tests/isa/rv64ui-p-xori.bin")))

(define res-instruction-check (run-tests riscv-tests))
