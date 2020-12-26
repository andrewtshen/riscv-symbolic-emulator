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

(define (assert-OK m)
	; Assert that code is in an OK state as described by the paper in the ./report folder.

	; mode is not always equal, do not assert
	; OK property
	(assert (bveq (get-csr m 'mtvec) (bv #x0000000080000080 64)))
	(assert (bveq (get-csr m 'pmpcfg0) (bv #x000000000000001f 64)))
	(assert (bveq (get-csr m 'pmpcfg2) (bv #x0000000000000018 64)))
	(assert (bveq (get-csr m 'pmpaddr0) (bv #x000000002000bfff 64)))
	(assert (bveq (get-csr m 'pmpaddr1) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr1) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr2) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr3) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr4) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr5) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr6) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr7) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr8) (bv #x7fffffffffffffff 64)))
	(assert (bveq (get-csr m 'pmpaddr9) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr10) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr11) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr12) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr13) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr14) (bv #x0 64)))
	(assert (bveq (get-csr m 'pmpaddr15) (bv #x0 64))))
(provide assert-OK)

(define (assert-csr-equal m1 m2)
	; Assert that the CSRs are equal between two machines m1 and m2

	; mode is not always equal, do not assert
	; (assert (bveq (get-csr m1 'mode) (get-csr m2 'mode)))
	(assert (bveq (get-csr m1 'mtvec) (get-csr m2 'mtvec)))
	(assert (bveq (get-csr m1 'mepc) (get-csr m2 'mepc)))
	(assert (bveq (get-csr m1 'pmpcfg0) (get-csr m2 'pmpcfg0)))
	(assert (bveq (get-csr m1 'pmpcfg2) (get-csr m2 'pmpcfg2)))
	(assert (bveq (get-csr m1 'pmpaddr0) (get-csr m2 'pmpaddr0)))
	(assert (bveq (get-csr m1 'pmpaddr1) (get-csr m2 'pmpaddr1)))
	(assert (bveq (get-csr m1 'pmpaddr2) (get-csr m2 'pmpaddr2)))
	(assert (bveq (get-csr m1 'pmpaddr3) (get-csr m2 'pmpaddr3)))
	(assert (bveq (get-csr m1 'pmpaddr4) (get-csr m2 'pmpaddr4)))
	(assert (bveq (get-csr m1 'pmpaddr5) (get-csr m2 'pmpaddr5)))
	(assert (bveq (get-csr m1 'pmpaddr6) (get-csr m2 'pmpaddr6)))
	(assert (bveq (get-csr m1 'pmpaddr7) (get-csr m2 'pmpaddr7)))
	(assert (bveq (get-csr m1 'pmpaddr8) (get-csr m2 'pmpaddr8))))
(provide assert-csr-equal)

(define (deep-copy-machine m)
	; Create a deep copy of machine m
	(machine
		(cpu
			(csrs 
				(get-csr m 'mtvec)
				(get-csr m 'mepc)
				(get-csr m 'mstatus)
				(get-csr m 'pmpcfg0)
				(get-csr m 'pmpcfg2)
				(get-csr m 'pmpaddr0)
				(get-csr m 'pmpaddr1)
				(get-csr m 'pmpaddr2)
				(get-csr m 'pmpaddr3)
				(get-csr m 'pmpaddr4)
				(get-csr m 'pmpaddr5)
				(get-csr m 'pmpaddr6)
				(get-csr m 'pmpaddr7)
				(get-csr m 'pmpaddr8)
				(get-csr m 'pmpaddr9)
				(get-csr m 'pmpaddr10)
				(get-csr m 'pmpaddr11)
				(get-csr m 'pmpaddr12)
				(get-csr m 'pmpaddr13)
				(get-csr m 'pmpaddr14)
				(get-csr m 'pmpaddr15))
			(for/vector ([i (cpu-gprs (machine-cpu m))])
				i)
			(get-pc m))
		(machine-ram m)
			(machine-mode m)))
(provide deep-copy-machine)

(define (assert-mem-equal m1 m2 pos)
	; Assert that memory is equal between machines m1 and m2
	(assert (bveq (memory-read (machine-ram m1) pos) (memory-read (machine-ram m2) pos))))
(provide assert-mem-equal)

;; Sanity Checks for Individual Instructions

(define-test-suite instruction-check
	(test-case "add test"
		(define program (file->bytearray "build/add.bin"))
    (printf "* Running add.bin test ~n")
		(define m (parameterize
			([use-debug-mode #f])
			(init-machine-with-prog program)))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m))
		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		(define model_add (verify (begin 
			(assert (bveq (bvadd (list-ref gprsx 5) (list-ref gprsx 7))
										(list-ref gprsx 6))))))
		(check-true (unsat? model_add)))
	(test-case "addi test"
		(define program (file->bytearray "build/addi.bin"))
		(printf "* Running addi.bin test ~n")
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m))
		(define gprsx (for/list ([i (in-range 10 18)])
										(gprs-get-x m i)))
		(define model_addi (verify (begin 
			(assert (bveq (list-ref gprsx 6)
										(bvadd (list-ref gprsx 5) (bv 32 64)))))))
		  (check-true (unsat? model_addi)))
	(test-case "addw test"
		(define program (file->bytearray "build/addw.bin"))
		(printf "* Running addw.bin test ~n")
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m))
		(define gprsx (for/list ([i (in-range 10 18)])
										(gprs-get-x m i)))
		(define model_addw (verify (begin 
			(assert (bveq (list-ref gprsx 6)
										(bvadd (list-ref gprsx 5) (list-ref gprsx 3)))))))
		(check-true (unsat? model_addw)))
	(test-case "sub test"
		(define program (file->bytearray "build/sub.bin"))
		(printf "* Running sub.bin test ~n")
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m)))
	(test-case "jal test"
		(define program (file->bytearray "build/jal.bin"))
		(printf "* Running jal.bin test ~n")
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m)))
	(test-case "sd/ld test"
		(define program (file->bytearray "build/sd_ld.bin"))
		(printf "* Running sd_ld.bin test ~n")
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m))
		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		; doubleword, use all bits
		(define model_sd_ld (verify (begin 
			(assert (bveq (list-ref gprsx 2) (list-ref gprsx 3))))))

		(check-true (unsat? model_sd_ld)))
	(test-case "sw/lw test"
		(define program (file->bytearray "build/sw_lw.bin"))
		(printf "* Running sw_lw.bin test ~n")
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m))

		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))

		; word, index into the 32 lower bits
		(define model_sw_lw (verify (begin 
			(assert (bveq (extract 31 0 (list-ref gprsx 2))
				(extract 31 0 (list-ref gprsx 3)))))))
		(check-true (unsat? model_sw_lw)))
	(test-case "sh/lh test"
		(define program (file->bytearray "build/sh_lh.bin"))
		(printf "* Running sh_lh.bin test ~n")
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m))
		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		; half-word, index into the 15 lower bits
		(define model_sh_lh (verify (begin 
			(assert (bveq (extract 15 0 (list-ref gprsx 2))
				(extract 15 0 (list-ref gprsx 3)))))))
		(check-true (unsat? model_sh_lh)))
	(test-case "sb/lb test"
		(define program (file->bytearray "build/sb_lb.bin"))
		(printf "* Running sb_lb.bin test ~n")
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m))
		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		; half-word, index into the 15 lower bits
		(define model_sb_lb (verify (begin 
			(assert (bveq (extract 7 0 (list-ref gprsx 2))
				(extract 7 0 (list-ref gprsx 3)))))))
		(check-true (unsat? model_sb_lb)))
  (test-case "srliw test"
    (define program (file->bytearray "build/srliw.bin"))
    (printf "* Running srliw.bin test ~n" )
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m))
		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		(define model_srliw (verify (begin 
			(assert (and (bveq (list-ref gprsx 1) (bv #xffffffffffffffff 64))
										(bveq (list-ref gprsx 2) (bv #x000000007fffffff 64))
										(bveq (list-ref gprsx 3) (bv #x0000000001ffffff 64))
										(bveq (list-ref gprsx 4) (bv #x000000000003ffff 64))
										(bveq (list-ref gprsx 5) (bv #x0000000000000001 64)))))))
		(check-true (unsat? model_srliw)))
	(test-case "addiw test"
    (define program (file->bytearray "build/addiw.bin"))
    (printf "* Running addiw.bin test ~n")
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m))
		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		(define model_addiw (verify (begin 
			(assert (and (bveq (list-ref gprsx 1) (bv #x000000007fffffff 64))
										(bveq (list-ref gprsx 2) (bv #xffffffff800007fe 64))
										(bveq (list-ref gprsx 3) (bv #xffffffffffffffff 64))
										(bveq (list-ref gprsx 4) (bv #x0000000000000000 64))
										(bveq (list-ref gprsx 5) (bv #xfffffffffffffffe 64)))))))
		(check-true (unsat? model_addiw))))

;; Sanity Checks for High Level OS Functionality

(define-test-suite high-level-test
	(test-case "stack test"
		; Run code that sets up a stack
		(printf "* Running stack.bin test ~n" )
  	(define program (file->bytearray "build/stack.bin"))
  	(define m (init-machine-with-prog program))
  	(execute-until-mret m))
	(test-case "pmp test"
		; Test PMP Set up
		(define program (file->bytearray "build/pmp.bin"))
		(printf "* Running pmp.bin test ~n")
		(define m (init-machine-with-prog program))
		(parameterize
			([use-debug-mode #f])
			(execute-until-mret m))
		(pmp-check m (bv #x00700001 64) (bv #x007FFFFF 64))))

;; Sanity Checks for Misc. Utilities

(define-test-suite utils
	(test-case "ctz64"
		(assert (check-equal? 8 (ctz64 (bv #xffffffffffffff00 64))))
		(assert (check-equal? 7 (ctz64 (bv #xffffffffffffff80 64))))
		(assert (check-equal? 2 (ctz64 (bv #xfffffffffffffff4 64))))
		(assert (check-equal? 63 (ctz64 (bv #x8000000000000000 64))))
		(assert (check-equal? 0 (ctz64 (bv #xffffffffffffffff 64))))
		(assert (check-equal? 0 (ctz64 (bv #x0000000000000000 64)))))
	(test-case "pmp check"
		(printf "* Running pmp.bin test ~n")
		(define program (file->bytearray "build/pmp.bin"))
		(define m (init-machine-with-prog program))
		(execute-until-mret m)
		(check-true (pmp-check m (bv #x80800000 64) (bv #x80800000 64)))
		(check-true (pmp-check m (bv #x80FFFFFF 64) (bv #x80FFFFFF 64)))
		(check-equal? (pmp-check m (bv #x80FFFFFF 64) (bv #x81000000 64)) #f)
		(check-equal? (pmp-check m (bv #x807FFFFF 64) (bv #x81000000 64)) #f)
		(check-equal? (not (pmp-check m (bv #x00700001 64) (bv #x007FFFFF 64))) #t) ; disabled uart
		(check-true (pmp-check m (bv #x10700001 64) (bv #x107FFFFF 64)))
		(check-equal? (pmp-check m (bv #x00700001 64) (bv #x107FFFFF 64)) #f)
		(check-true (equal? (machine-mode m) 0)))
	(test-case "pmp-napot-settings"
		; Test cases for decoding PMP configurations
		(define setting1 (pmp-decode-cfg (bv #x0000000000001f1f 64) 1))
		(check-equal? (list-ref setting1 1) 1)
		(check-equal? (list-ref setting1 2) 1)
		(check-equal? (list-ref setting1 3) 1)
		(define setting2 (pmp-decode-cfg (bv #x0000000000001f1f 64) 2))
		(check-equal? (list-ref setting2 1) 0)
		(check-equal? (list-ref setting2 2) 0)
		(check-equal? (list-ref setting2 3) 0)
		(define setting5 (pmp-decode-cfg (bv #x0000000000001f1f 64) 5))
		(check-equal? (list-ref setting5 1) 0)
		(check-equal? (list-ref setting5 2) 0)
		(check-equal? (list-ref setting5 3) 0))
	(test-case "decoding-instr-edge cases"
		(printf "* decoding-instr-edge cases ~n")
		(define m (init-machine))
		(check-equal? (decode m (bv #xffffffff 32)) null)
		(check-equal? (list-ref (decode m (bv #x0107c663 32)) 0) 'blt)
		; check decoding
		(check-equal? (list-ref (decode m (bv #x00000117 32)) 0) 'auipc)
		; check that produces null op if not applicable opcode
		(check-equal? (decode m (bv #b11111111111111111111111110110011 32)) null))
	(test-case "decoding-uncoded-instrs"
		(printf "* decoding-uncoded-instrs ~n")
		(define program (file->bytearray "build/dret.bin"))
		(define m (init-machine-with-prog program))
		(step m)))

;; Sanity Checks for Steps

(define-test-suite step-checks
	(test-case "memory tests"
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

		; Currently PMP allows user to only write in the region 0x0 --> 0x1FFFF
		(clear-asserts!)
		(define model_noninterference (verify
			#:assume
			(assert (and (not (bvule (bv #x20000 32) sym-idx) (bvule sym-idx (bv #x3FFFF 32)))))
			#:guarantee
			(assert-mem-equal m m1 sym-idx)))
		(check-true (unsat? model_noninterference))

		; Check the upper/lower bounds of the user region
		(clear-asserts!)
		(define model_ubound (verify
			#:assume
			(assert (bveq sym-idx (bv #x3FFFF 32)))
			#:guarantee
			(assert-mem-equal m m1 sym-idx)))
		(check-true (not (unsat? model_ubound)))

		(clear-asserts!)
		(define model_lbound (verify
			#:assume
			(assert (bveq sym-idx (bv #x20000 32)))
			#:guarantee
			(assert-mem-equal m m1 sym-idx)))
		(check-true (not (unsat? model_lbound))))
	(test-case "mode test"
		(printf "* Running mode tests ~n")
		(define m (parameterize
			([ramsize-log2 32])
			(init-machine)))
		(define m1 (deep-copy-machine m))

		(define next_instr (parameterize
			([use-sym-optimizations #f]
			[use-debug-mode #f]
			[ramsize-log2 32])
			(step m)))

		(clear-asserts!)
		(define model_mode (verify
			(assert (or (equal? (machine-mode m) (machine-mode m1))
									(and (bveq (get-pc m) (bvsub (get-csr m 'mtvec) (base-address))) (equal? (machine-mode m) 1))))))
		(check-true (unsat? model_mode))))

;; Test Case for Base Case

(define-test-suite boot-sequence
	(test-case "boot sequence test"
		(printf "* Running boot sequence test ~n")
		(define program (file->bytearray "kernel/kernel.bin"))
		(define m (parameterize
			([use-sym-optimizations #f]
			 [use-debug-mode #f]
			 [use-fnmem #f])
			(init-machine-with-prog program)))
		(parameterize
			([use-sym-optimizations #f]
			 [use-debug-mode #f]
			 [use-fnmem #f])
			(execute-until-mret m))

		; Check that after boot sequence machine mode is user mode (0) and in OK state
		(print-pmp m)
		(check-true (equal? (machine-mode m) 0))
		(assert-OK m)))

;; Test Case for Inductive Step

(define-test-suite inductive-step
	(test-case "inductive step test"
		(printf "* Running inductive step test ~n")
		(define m (parameterize
			([ramsize-log2 20])
			(init-machine)))
		(define m1 (deep-copy-machine m))

		(define next_instr (parameterize
			([use-sym-optimizations #f]
			[use-debug-mode #f]
			[ramsize-log2 20])
			(step m)))

		; Check that mode of m1 is either equal to the mtvec or user mode
		(define model_mode (verify
			(assert (or (equal? (machine-mode m) (machine-mode m1))
									(and (bveq (get-pc m) (bvsub (get-csr m 'mtvec) (base-address))) (equal? (machine-mode m) 1))))))
		(check-true (unsat? model_mode))

		; Check that m1 is in an OK state
		(clear-asserts!)
		(define model_OK (verify
			(assert-OK m1)))
		(check-true (unsat? model_OK))

		; Check that memory between m and m1 is same except for in user memory (0x20000 -> 0x3FFFF)
		(clear-asserts!)
		(define-symbolic* sym-idx (bitvector 20))
		(define model_noninterference (verify
			#:assume
			(assert (and (not (bvule (bv #x20000 32) sym-idx) (bvule sym-idx (bv #x3FFFF 32)))))
			#:guarantee
			(assert-mem-equal m m1 sym-idx)))
			(check-true (unsat? model_noninterference))))

(define res-instruction-check (run-tests instruction-check))
(define res-utils (run-tests utils))
(define res-high-level-test (run-tests high-level-test))
(define res-step-checks (run-tests step-checks))

;; Testing the base case and inductive step

; (define res-boot-sequence (run-tests boot-sequence))
(define res-inductive-step (run-tests inductive-step))
; (define res-boot-sequence (time (run-tests boot-sequence)))
; (define res-inductive-step (time (run-tests inductive-step)))
