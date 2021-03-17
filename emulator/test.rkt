#lang rosette/safe

(require
  "init.rkt"
  "emulate.rkt"
  "machine.rkt"
  "pmp.rkt"
  "execute.rkt"
  "parameters.rkt"
  "print-utils.rkt"
  "instr.rkt")
(require (only-in racket/base 
                  custodian-limit-memory current-custodian parameterize call-with-parameterization
                  parameterize* for for/list for/vector in-range))
(require rackunit rackunit/text-ui)
(require profile)

(define (assert-OK m)
  ; Assert that code is in an OK state as described by the paper in the ./report folder.
  
  ; mode is not always equal, do not assert
  ; OK property
  (assert (bveq (machine-csr m 'mtvec) (bv #x0000000080000080 64)))
  (assert (bveq (machine-csr m 'pmpcfg0) (bv #x000000000000001f 64)))
  (assert (bveq (machine-csr m 'pmpcfg2) (bv #x0000000000000018 64)))
  (assert (bveq (machine-csr m 'pmpaddr0) (bv #x000000002000bfff 64)))
  (assert (bveq (machine-csr m 'pmpaddr1) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr2) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr3) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr4) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr5) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr6) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr7) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr8) (bv #x7fffffffffffffff 64)))
  (assert (bveq (machine-csr m 'pmpaddr9) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr10) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr11) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr12) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr13) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr14) (bv #x0 64)))
  (assert (bveq (machine-csr m 'pmpaddr15) (bv #x0 64))))
(provide assert-OK)

(define (assert-csr-equal m1 m2)
  ; Assert that the CSRs are equal between two machines m1 and m2
  
  ; mode is not always equal, do not assert
  ; (assert (bveq (machine-csr m1 'mode) (machine-csr m2 'mode)))
  (assert (bveq (machine-csr m1 'mtvec) (machine-csr m2 'mtvec)))
  (assert (bveq (machine-csr m1 'mepc) (machine-csr m2 'mepc)))
  (assert (bveq (machine-csr m1 'pmpcfg0) (machine-csr m2 'pmpcfg0)))
  (assert (bveq (machine-csr m1 'pmpcfg2) (machine-csr m2 'pmpcfg2)))
  (assert (bveq (machine-csr m1 'pmpaddr0) (machine-csr m2 'pmpaddr0)))
  (assert (bveq (machine-csr m1 'pmpaddr1) (machine-csr m2 'pmpaddr1)))
  (assert (bveq (machine-csr m1 'pmpaddr2) (machine-csr m2 'pmpaddr2)))
  (assert (bveq (machine-csr m1 'pmpaddr3) (machine-csr m2 'pmpaddr3)))
  (assert (bveq (machine-csr m1 'pmpaddr4) (machine-csr m2 'pmpaddr4)))
  (assert (bveq (machine-csr m1 'pmpaddr5) (machine-csr m2 'pmpaddr5)))
  (assert (bveq (machine-csr m1 'pmpaddr6) (machine-csr m2 'pmpaddr6)))
  (assert (bveq (machine-csr m1 'pmpaddr7) (machine-csr m2 'pmpaddr7)))
  (assert (bveq (machine-csr m1 'pmpaddr8) (machine-csr m2 'pmpaddr8))))
(provide assert-csr-equal)

(define (deep-copy-machine m)
  ; Create a deep copy of machine m
  (machine
    (cpu
      (csrs 
        (machine-csr m 'mtvec)
        (machine-csr m 'mepc)
        (machine-csr m 'mstatus)
        (pmp
          (for/vector ([p (pmp-pmpcfgs (csrs-pmp (cpu-csrs (machine-cpu m))))])
            (pmpcfg
              (pmpcfg-value p)
              (for/vector ([s (pmpcfg-settings p)])
                (pmpcfg-setting
                  (pmpcfg-setting-R s)
                  (pmpcfg-setting-W s)
                  (pmpcfg-setting-X s)
                  (pmpcfg-setting-A s)
                  (pmpcfg-setting-L s)))))
          (for/vector ([p (pmp-pmpaddrs (csrs-pmp (cpu-csrs (machine-cpu m))))])
            (pmpaddr
              (pmpaddr-value p)
              (pmpaddr-startaddr p)
              (pmpaddr-endaddr p)))
          (pmp-numimplemented (csrs-pmp (cpu-csrs (machine-cpu m))))))
      (for/vector ([i (cpu-gprs (machine-cpu m))])
        i)
      (machine-pc m))
    (machine-ram m)
    (machine-mode m)))
(provide deep-copy-machine)

(define (assert-mem-equal m1 m2 pos)
  ; Assert that memory is equal between machines m1 and m2
  (assert (bveq (memory-read (machine-ram m1) pos)
                (memory-read (machine-ram m2) pos))))
(provide assert-mem-equal)

;; Sanity Checks for Individual Instructions

(define-test-suite
  instruction-check
  (test-case "add test"
             (clear-terms!)
             (define program (file->bytearray "build/add.bin"))
             (printf "* Running add.bin test ~n")
             (define m
               (parameterize
                 ([use-debug-mode #f])
                 (init-machine-with-prog program)))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m))
             (define gprsx
               (for/list ([i (in-range 10 18)])
                 (get-gprs-i (machine-gprs m) i)))
             (define model_add
               (verify (begin (assert
                                (bveq (bvadd (list-ref gprsx 5) (list-ref gprsx 7))
                                      (list-ref gprsx 6))))))
             (check-true (unsat? model_add)))
  (test-case "addi test"
             (clear-terms!)
             (define program (file->bytearray "build/addi.bin"))
             (printf "* Running addi.bin test ~n")
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m))
             (define gprsx
               (for/list ([i (in-range 10 18)])
                 (get-gprs-i (machine-gprs m) i)))
             (define model_addi
               (verify (begin (assert
                                (bveq (list-ref gprsx 6)
                                      (bvadd (list-ref gprsx 5) (bv 32 64)))))))
             (check-true (unsat? model_addi)))
  (test-case "addw test"
             (clear-terms!)
             (define program (file->bytearray "build/addw.bin"))
             (printf "* Running addw.bin test ~n")
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m))
             (define gprsx
               (for/list ([i (in-range 10 18)])
                 (get-gprs-i (machine-gprs m) i)))
             (define model_addw
               (verify (begin (assert
                                (bveq (list-ref gprsx 6)
                                      (bvadd (list-ref gprsx 5) (list-ref gprsx 3)))))))
             (check-true (unsat? model_addw)))
  (test-case "sub test"
             (clear-terms!)
             (define program (file->bytearray "build/sub.bin"))
             (printf "* Running sub.bin test ~n")
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m)))
  (test-case "jal test"
             (clear-terms!)
             (define program (file->bytearray "build/jal.bin"))
             (printf "* Running jal.bin test ~n")
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m)))
  (test-case "sd/ld test"
             (clear-terms!)
             (define program (file->bytearray "build/sd_ld.bin"))
             (printf "* Running sd_ld.bin test ~n")
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m))
             (define gprsx
               (for/list ([i (in-range 10 18)])
                 (get-gprs-i (machine-gprs m) i)))
             ; doubleword, use all bits
             (define model_sd_ld
               (verify (begin (assert
                                (bveq (list-ref gprsx 2) (list-ref gprsx 3))))))
             
             (check-true (unsat? model_sd_ld)))
  (test-case "sw/lw test"
             (clear-terms!)
             (define program (file->bytearray "build/sw_lw.bin"))
             (printf "* Running sw_lw.bin test ~n")
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m))
             (define gprsx
               (for/list ([i (in-range 10 18)])
                 (get-gprs-i (machine-gprs m) i)))
             ; word, index into the 32 lower bits
             (define model_sw_lw
               (verify (begin (assert
                                (bveq (extract 31 0 (list-ref gprsx 2))
                                      (extract 31 0 (list-ref gprsx 3)))))))
             (check-true (unsat? model_sw_lw)))
  (test-case "sh/lh test"
             (clear-terms!)
             (define program (file->bytearray "build/sh_lh.bin"))
             (printf "* Running sh_lh.bin test ~n")
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m))
             (define gprsx
               (for/list ([i (in-range 10 18)])
                 (get-gprs-i (machine-gprs m) i)))
             ; half-word, index into the 15 lower bits
             (define model_sh_lh
               (verify (begin (assert
                                (bveq (extract 15 0 (list-ref gprsx 2))
                                      (extract 15 0 (list-ref gprsx 3)))))))
             (check-true (unsat? model_sh_lh)))
  (test-case "sb/lb test"
             (clear-terms!)
             (define program (file->bytearray "build/sb_lb.bin"))
             (printf "* Running sb_lb.bin test ~n")
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m))
             (define gprsx
               (for/list ([i (in-range 10 18)])
                 (get-gprs-i (machine-gprs m) i)))
             ; half-word, index into the 15 lower bits
             (define model_sb_lb
               (verify (begin (assert
                                (bveq (extract 7 0 (list-ref gprsx 2))
                                      (extract 7 0 (list-ref gprsx 3)))))))
             (check-true (unsat? model_sb_lb)))
  (test-case "srliw test"
             (clear-terms!)
             (define program (file->bytearray "build/srliw.bin"))
             (printf "* Running srliw.bin test ~n" )
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m))
             (define gprsx
               (for/list ([i (in-range 10 18)])
                 (get-gprs-i (machine-gprs m) i)))
             (define model_srliw
               (verify (begin (assert
                                (and (bveq (list-ref gprsx 1) (bv #xffffffffffffffff 64))
                                     (bveq (list-ref gprsx 2) (bv #x000000007fffffff 64))
                                     (bveq (list-ref gprsx 3) (bv #x0000000001ffffff 64))
                                     (bveq (list-ref gprsx 4) (bv #x000000000003ffff 64))
                                     (bveq (list-ref gprsx 5) (bv #x0000000000000001 64)))))))
             (check-true (unsat? model_srliw)))
  (test-case "addiw test"
             (clear-terms!)
             (define program (file->bytearray "build/addiw.bin"))
             (printf "* Running addiw.bin test ~n")
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m))
             (define gprsx
               (for/list ([i (in-range 10 18)])
                 (get-gprs-i (machine-gprs m) i)))
             (define model_addiw
               (verify (begin (assert (and (bveq (list-ref gprsx 1) (bv #x000000007fffffff 64))
                                           (bveq (list-ref gprsx 2) (bv #xffffffff800007fe 64))
                                           (bveq (list-ref gprsx 3) (bv #xffffffffffffffff 64))
                                           (bveq (list-ref gprsx 4) (bv #x0000000000000000 64))
                                           (bveq (list-ref gprsx 5) (bv #xfffffffffffffffe 64)))))))
             (check-true (unsat? model_addiw))))

;; Sanity Checks for High Level OS Functionality

(define-test-suite
  high-level-test
  (test-case "stack test"
             (clear-terms!)
             ; Run code that sets up a stack
             (printf "* Running stack.bin test ~n" )
             (define program (file->bytearray "build/stack.bin"))
             (define m (init-machine-with-prog program))
             (execute-until-mret m))
  (test-case "pmp test"
             (clear-terms!)
             ; Test PMP Set up
             (define program (file->bytearray "build/pmp.bin"))
             (printf "* Running pmp.bin test ~n")
             (define m (init-machine-with-prog program))
             (parameterize
               ([use-debug-mode #f])
               (execute-until-mret m))
             (pmp-check (machine-pmp m) (machine-mode m)
                        (bv #x00700001 64) (bv #x007FFFFF 64)))
  (test-case "kernel test (with concrete optimizations)"
             (clear-terms!)
             (printf "* Running kernel test (with concrete optimizations) ~n")
             (define program (file->bytearray "kernel/kernel.bin"))
             (define m
               (parameterize
                 ([use-sym-optimizations #f]
                  [use-debug-mode #f]
                  [use-fnmem #f]
                  [use-concrete-optimizations #t])
                 (init-machine-with-prog program)))
             (parameterize
               ([use-sym-optimizations #f]
                [use-debug-mode #f]
                [use-fnmem #f]
                [use-concrete-optimizations #t])
               (execute-until-mret m))

             ; Check that after boot sequence machine mode is user mode (0) and in OK state
             (check-true (equal? (machine-mode m) 0))
             (assert-OK m))
  (test-case "kernel test (no concrete optimizations)"
             (clear-terms!)
             (printf "* Running kernel test (no concrete optimizations) ~n")
             (define program (file->bytearray "kernel/kernel.bin"))
             (define m
               (parameterize
                 ([use-sym-optimizations #f]
                  [use-debug-mode #f]
                  [use-fnmem #f]
                  [use-concrete-optimizations #f])
                 (init-machine-with-prog program)))
             (parameterize
               ([use-sym-optimizations #f]
                [use-debug-mode #f]
                [use-fnmem #f]
                [use-concrete-optimizations #f])
               (execute-until-mret m))

             ; Check that after boot sequence machine mode is user mode (0) and in OK state
             (check-true (equal? (machine-mode m) 0))
             (assert-OK m)))

;; Sanity Checks for Misc. Utilities

(define-test-suite
  utils
  (test-case "ctz64"
             (clear-terms!)
             (assert (check-equal? 8  (ctz64 (bv #xffffffffffffff00 64))))
             (assert (check-equal? 7  (ctz64 (bv #xffffffffffffff80 64))))
             (assert (check-equal? 2  (ctz64 (bv #xfffffffffffffff4 64))))
             (assert (check-equal? 63 (ctz64 (bv #x8000000000000000 64))))
             (assert (check-equal? 0  (ctz64 (bv #xffffffffffffffff 64))))
             (assert (check-equal? 0  (ctz64 (bv #x0000000000000000 64)))))
  (test-case "pmp check"
             (clear-terms!)
             (printf "* Running pmp.bin test ~n")
             (define program (file->bytearray "build/pmp.bin"))
             (define m (init-machine-with-prog program))
             (execute-until-mret m)
             ; (print-pmp m)
             (check-equal? (machine-mode m) 0)
             (check-true (pmp-check (machine-pmp m) (machine-mode m)
                                    (bv #x80800000 64) (bv #x80800000 64)))
             (check-true (pmp-check (machine-pmp m) (machine-mode m)
                                    (bv #x80FFFFFF 64) (bv #x80FFFFFF 64)))
             (check-equal? (pmp-check (machine-pmp m) (machine-mode m)
                                      (bv #x80FFFFFF 64) (bv #x81000000 64)) #f)
             (check-equal? (pmp-check (machine-pmp m) (machine-mode m)
                                      (bv #x807FFFFF 64) (bv #x81000000 64)) #f)
             ; disabled uart
             (check-equal? (not (pmp-check (machine-pmp m) (machine-mode m)
                                           (bv #x00700001 64) (bv #x007FFFFF 64))) #t) 
             (check-true (pmp-check (machine-pmp m) (machine-mode m)
                                    (bv #x10700001 64) (bv #x107FFFFF 64)))
             (check-equal? (pmp-check (machine-pmp m) (machine-mode m)
                                      (bv #x00700001 64) (bv #x107FFFFF 64)) #f)
             (check-true (equal? (machine-mode m) 0))
             (check-equal? (pmp-numimplemented (machine-pmp m)) 3)
             (check-true (not (equal? (pmp-numimplemented (machine-pmp m)) 4)))
             (check-true (not (equal? (pmp-numimplemented (machine-pmp m)) 5)))
             (check-true (not (equal? (pmp-numimplemented (machine-pmp m)) 1))))
  (test-case "pmp-napot-settings"
             (clear-terms!)
             ; Test cases for decoding PMP configurations
             (define setting1 (pmp-decode-cfg (bv #x0000000000001f1f 64) 1))
             (check-equal? (pmpcfg-setting-R setting1) (bv 1 1))
             (check-equal? (pmpcfg-setting-W setting1) (bv 1 1))
             (check-equal? (pmpcfg-setting-A setting1) (bv 3 2))
             (check-equal? (pmpcfg-setting-X setting1) (bv 1 1))
             (check-equal? (pmpcfg-setting-L setting1) (bv 0 1))
             (define setting2 (pmp-decode-cfg (bv #x0000000000001f1f 64) 2))
             (check-equal? (pmpcfg-setting-R setting2) (bv 0 1))
             (check-equal? (pmpcfg-setting-W setting2) (bv 0 1))
             (check-equal? (pmpcfg-setting-A setting2) (bv 0 2))
             (check-equal? (pmpcfg-setting-X setting2) (bv 0 1))
             (check-equal? (pmpcfg-setting-L setting2) (bv 0 1))
             (define setting5 (pmp-decode-cfg (bv #x0000000000001f1f 64) 5))
             (check-equal? (pmpcfg-setting-R setting5) (bv 0 1))
             (check-equal? (pmpcfg-setting-W setting5) (bv 0 1))
             (check-equal? (pmpcfg-setting-A setting5) (bv 0 2))
             (check-equal? (pmpcfg-setting-X setting5) (bv 0 1))
             (check-equal? (pmpcfg-setting-L setting5) (bv 0 1)))
  (test-case "decoding-instr-edge cases"
             (clear-terms!)
             (printf "* decoding-instr-edge cases ~n")
             (define m (init-machine))
             (check-equal? (execute m (bv #xffffffff 32)) 'illegal-instruction)
             (check-equal? (list-ref (execute m (bv #x0107c663 32)) 0) 'blt)
             ; check decoding
             (check-equal? (list-ref (execute m (bv #x00000117 32)) 0) 'auipc)
             ; check that produces illegal instruction if cannot decode
             (check-equal? (execute m (bv #b11111111111111111111111110110011 32)) 'illegal-instruction)
             (check-equal? (execute m (bv #x0 32)) 'illegal-instruction))
  (test-case "decoding-uncoded-instrs"
             (clear-terms!)
             (printf "* decoding-uncoded-instrs ~n")
             (define program (file->bytearray "build/dret.bin"))
             (define m (init-machine-with-prog program))
             (step m)))

;; Sanity Checks for Steps

(define-test-suite
  step-checks
  (test-case "memory tests"
             (clear-terms!)
             (printf "* Running memory tests ~n")
             (define m (parameterize
                         ([ramsize-log2 32])
                         (init-machine)))
             (define m1 (deep-copy-machine m))
             
             (define next_instr (parameterize
                                  ([use-sym-optimizations #f]
                                   [use-debug-mode #f]
                                   [ramsize-log2 32])
                                  (step m)))             
             (define-symbolic* sym-idx (bitvector 32))
             
             ; Currently PMP allows user to only write in the region 0x20000 --> 0x3FFFF
             (define model_noninterference
               (verify
                 (begin
                   (assume (or (bvult sym-idx (bv #x20000 32)) (bvult (bv #x3FFFF 32) sym-idx)))
                   (assert-mem-equal m m1 sym-idx))))
             (check-true (unsat? model_noninterference))
             
             ; Check the upper/lower bounds of the user region
             (clear-vc!)
             (define model_ubound
               (verify
                 (begin
                   (assume (bveq sym-idx (bv #x3FFFF 32)))
                   (assert-mem-equal m m1 sym-idx))))
             (check-true (not (unsat? model_ubound)))
             
             (clear-vc!)
             (define model_lbound
               (verify
                 (begin
                   (assume (bveq sym-idx (bv #x20000 32)))
                   (assert-mem-equal m m1 sym-idx))))
             (check-true (not (unsat? model_lbound))))
  (test-case "mode test"
             (clear-terms!)
             (printf "* Running mode tests ~n")
             (define m
               (parameterize
                 ([ramsize-log2 32])
                 (init-machine)))
             (define m1 (deep-copy-machine m))
             
             (define next_instr
               (parameterize
                 ([use-sym-optimizations #f]
                  [use-debug-mode #f]
                  [ramsize-log2 32])
                 (step m)))
             
             (clear-vc!)
             (define model_mode
               (verify (assert
                         (or (equal? (machine-mode m) (machine-mode m1))
                             (and (bveq (machine-pc m)
                                        (bvsub (machine-csr m 'mtvec) (base-address)))
                                  (equal? (machine-mode m) 1))))))
             (check-true (unsat? model_mode)))
  (test-case "only user mode test"
             (clear-terms!)
             (printf "* Running only user mode test ~n")
             (define m
               (parameterize
                 ([ramsize-log2 32])
                 (init-machine)))
             (define m1 (deep-copy-machine m))
             
             (define next_instr
               (parameterize
                 ([use-sym-optimizations #f]
                  [use-debug-mode #f]
                  [ramsize-log2 32])
                 (step m)))
             
             (clear-vc!)
             (define model_only_user_mode
               (verify (assert (equal? (machine-mode m) 0))))
             (check-true (unsat? model_only_user_mode)))
  (test-case "cannot reach mtvec test"
             (clear-terms!)
             (printf "* Running cannot reach mtvec test ~n")
             (define m
               (parameterize
                 ([ramsize-log2 32])
                 (init-machine)))
             (define m1 (deep-copy-machine m))
             
             (define next_instr
               (parameterize
                 ([use-sym-optimizations #f]
                  [use-debug-mode #f]
                  [ramsize-log2 32])
                 (step m)))
             
             (clear-vc!)
             (define model_only_user_mode
               (verify (assert
                         (not (and (bveq (machine-pc m) (bvsub (machine-csr m 'mtvec) (base-address)))
                                   (equal? (machine-mode m) 1))))))
             (check-true (unsat? model_only_user_mode)))
  (test-case "does not return null test"
             (clear-terms!)
             (printf "* Running does not return null test ~n")
             (define m
               (parameterize
                 ([ramsize-log2 32])
                 (init-machine)))
             (define m1 (deep-copy-machine m))
             
             (define next_instr
               (parameterize
                 ([use-sym-optimizations #t]
                  [use-debug-mode #f]
                  [ramsize-log2 32])
                 (step m)))
             
             (clear-vc!)
             (define model_does_not_return_null
               (verify (assert
                         (not (equal? next_instr null)))))
             (check-true (unsat? model_does_not_return_null))
             (clear-vc!)
             (define model_does_return_illegal_instr
               (verify (assert (not (equal? next_instr 'illegal-instruction)))))
             (check-true (not (unsat? model_does_return_illegal_instr)))))

;; Test Case for Base Case

(define-test-suite
  boot-sequence
  (test-case "boot sequence test"
             (clear-terms!)
             (printf "* Running boot sequence test ~n")
             (define program (file->bytearray "kernel/ci/kernel.bin"))
             (define m
               (parameterize
                 ([use-sym-optimizations #f]
                  [use-debug-mode #f]
                  [use-fnmem #f]
                  [use-concrete-optimizations #f])
                 (init-machine-with-prog program)))
             (parameterize
               ([use-sym-optimizations #f]
                [use-debug-mode #f]
                [use-fnmem #f]
                [use-concrete-optimizations #f])
               (execute-until-mret m))
             
             ; Check that after boot sequence machine mode is user mode (0) and in OK state
             (check-true (equal? (machine-mode m) 0))
             (assert-OK m)))

;; Test Case for Inductive Step

(define-test-suite
  inductive-step
  (test-case "inductive step test"
             (clear-terms!)
             (printf "* Running inductive step test ~n")
             ; Create machine in the OK state
             (define m
               (parameterize
                 ([use-sym-optimizations #t]
                  [ramsize-log2 32])
                 (init-machine)))
             
             ; Create a copy of the machine and take arbitrary step
             (define m1 (deep-copy-machine m))
             (define next_instr
               (parameterize
                 ([use-sym-optimizations #t]
                  [ramsize-log2 32])
                 (step m)))
             
             ; Check that mode of m1 is either equal to the mtvec or user mode
             (define model_mode
               (verify (assert
                         (or (equal? (machine-mode m) (machine-mode m1))
                             (and (bveq (machine-pc m)
                                        (bvsub (machine-csr m 'mtvec) (base-address)))
                                  (equal? (machine-mode m) 1))))))
             (check-true (unsat? model_mode))
             
             ; Check that m1 is in an OK state
             (define model_OK (verify (assert-OK m1)))
             (check-true (unsat? model_OK))
             
             ; Check that memory between m and m1 is same except for in user memory;
             ; (0x20000 -> 0x3FFFF)
             ; TODO: check if it is 32 or 20 for mem access
             (define-symbolic* sym-idx (bitvector 32))
             (define model_noninterference
               (verify
                 (begin
                   (assume (or (bvult sym-idx (bv #x20000 32)) (bvult (bv #x3FFFF 32) sym-idx)))
                   (assert-mem-equal m m1 sym-idx))))
             (check-true (unsat? model_noninterference))))

(define res-instruction-check (run-tests instruction-check))
(define res-utils (run-tests utils))
(define res-high-level-test (run-tests high-level-test))
(define res-step-checks (run-tests step-checks))

;; Testing the base case and inductive step

(define res-boot-sequence (time (run-tests boot-sequence)))
(define res-inductive-step (time (run-tests inductive-step)))
