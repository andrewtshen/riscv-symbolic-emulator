#lang rosette/safe

(require
	"init.rkt"
	"emulate.rkt"
	"execute.rkt"
	"machine.rkt"
	"pmp.rkt")
(require (only-in racket/base for/list for/vector in-range))
(require rackunit rackunit/text-ui)

; Test Cases for Symbolic Executions
; note: currently need to add more test cases and fix symbolic variable construction
; 			to allow for the usage of more than one machine in order to run multiple programs.

(define-test-suite instruction-check
  (test-case "add test"
    (define program (file->bytearray "build/add.bin"))
    (printf "~n* Running add.bin test ~n" )
		; make machine
		(define ramsize 1000)
		(define m (init-machine program ramsize))
		(execute-until-mret m)

		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))

		(define model_add (verify (begin 
			(assert (bveq (bvadd (list-ref gprsx 5) (list-ref gprsx 7))
										(list-ref gprsx 6))))))
		(check-true (unsat? model_add)))
	(test-case "addi test"
		(define program (file->bytearray "build/addi.bin"))
		(define ramsize 1000)
		(printf "~n* Running addi.bin test ~n")
		(define m (init-machine program ramsize))
		(execute-until-mret m)
		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		(define model_addi (verify (begin 
			(assert (bveq (list-ref gprsx 6)
										(bvadd (list-ref gprsx 5) (bv 32 64)))))))

		(check-true (unsat? model_addi)))
	(test-case "addw test"
		(define program (file->bytearray "build/addw.bin"))
		(define ramsize 1000)
		(printf "~n* Running addw.bin test ~n")
		(define m (init-machine program ramsize))
		(execute-until-mret m)
		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		(define model_addw (verify (begin 
			(assert (bveq (list-ref gprsx 6)
										(bvadd (list-ref gprsx 5) (list-ref gprsx 3)))))))

		(check-true (unsat? model_addw)))
	(test-case "sub test"
		; TODO: make this into an actual test case
		(define program (file->bytearray "build/sub.bin"))
		(define ramsize 10000)
		(printf "~n* Running sub.bin test ~n")
		(define m (init-machine program ramsize))
		(execute-until-mret m))
	(test-case "jal test"
		(define program (file->bytearray "build/jal.bin"))
		(define ramsize 10000)
		(printf "~n* Running jal.bin test ~n")
		(define m (init-machine program ramsize))
		(execute-until-mret m))
	(test-case "sd/ld test"
		(define program (file->bytearray "build/sd_ld.bin"))
		(printf "~n* Running sd_ld.bin test ~n")
		(define ramsize 1000)
		(define m (init-machine program ramsize))
		(execute-until-mret m)

		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		; doubleword, use all bits
		(define model_sd_ld (verify (begin 
			(assert (bveq (list-ref gprsx 2) (list-ref gprsx 3))))))

		(check-true (unsat? model_sd_ld)))
	(test-case "sw/lw test"
		(define program (file->bytearray "build/sw_lw.bin"))
		(printf "~n* Running sw_lw.bin test ~n")
		(define ramsize 1000)
		(define m (init-machine program ramsize))
		(execute-until-mret m)

		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))

		; word, index into the 32 lower bits
		(define model_sw_lw (verify (begin 
			(assert (bveq (extract 31 0 (list-ref gprsx 2))
				(extract 31 0 (list-ref gprsx 3)))))))
		; (define model_lw_sw (verify (begin 
		; 	(assert (bveq (extract 32 0 (list-ref gprsx 2))
		; 		(extract 32 0 (list-ref gprsx 3)))))))
		(check-true (unsat? model_sw_lw)))
	(test-case "sh/lh test"
		(define program (file->bytearray "build/sh_lh.bin"))
		(printf "~n* Running sh_lh.bin test ~n")
		(define ramsize 1000)
		(define m (init-machine program ramsize))
		(execute-until-mret m)

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
		(printf "~n* Running sb_lb.bin test ~n")
		(define ramsize 1000)
		(define m (init-machine program ramsize))
		(execute-until-mret m)

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
    (printf "~n* Running srliw.bin test ~n" )
		; make machine
		(define ramsize 1000)
		(define m (init-machine program ramsize))
		(execute-until-mret m)

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
    (printf "~n* Running addiw.bin test ~n" )
		; make machine
		(define ramsize 1000)
		(define m (init-machine program ramsize))
		(execute-until-mret m)

		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		; (displayln gprsx)
		(define model_addiw (verify (begin 
			(assert (and (bveq (list-ref gprsx 1) (bv #x000000007fffffff 64))
										(bveq (list-ref gprsx 2) (bv #xffffffff800007fe 64))
										(bveq (list-ref gprsx 3) (bv #xffffffffffffffff 64))
										(bveq (list-ref gprsx 4) (bv #x0000000000000000 64))
										(bveq (list-ref gprsx 5) (bv #xfffffffffffffffe 64)))))))
		(check-true (unsat? model_addiw))))

(define-test-suite high-level-test
	(test-case "stack test"
  	(define program (file->bytearray "build/stack.bin"))
  	(printf "~n* Running stack.bin test ~n" )
  	(define ramsize 1000)
  	(define m (init-machine program ramsize))
  	(execute-until-mret m))
	(test-case "pmp test"
		(define program (file->bytearray "build/pmp.bin"))
		(printf "~n* Running pmp.bin test ~n")
		(define ramsize 10000)
		(define m (init-machine program ramsize))
		(execute-until-mret m)
		(print-pmp m)
		(pmp-check m (bv #x00700001 64) (bv #x007FFFFF 64))))

(define-test-suite utils
	(test-case "ctz64"
		; CTZ Tests
		(assert (check-equal? 8 (ctz64 (bv #xffffffffffffff00 64))))
		(assert (check-equal? 7 (ctz64 (bv #xffffffffffffff80 64))))
		(assert (check-equal? 2 (ctz64 (bv #xfffffffffffffff4 64))))
		(assert (check-equal? 0 (ctz64 (bv #xffffffffffffffff 64))))
		(assert (check-equal? 0 (ctz64 (bv #x0000000000000000 64)))))
	(test-case "pmp check"
		(define program (file->bytearray "build/pmp.bin"))
		(printf "~n* Running pmp.bin test ~n")
		(define ramsize 10000)
		(define m (init-machine program ramsize))
		(execute-until-mret m)
		(print-pmp m)
		(check-true (pmp-check m (bv #x80800000 64) (bv #x80800000 64)))
		(check-true (pmp-check m (bv #x80FFFFFF 64) (bv #x80FFFFFF 64)))
		(check-false (pmp-check m (bv #x80FFFFFF 64) (bv #x81000000 64)))
		(check-false (pmp-check m (bv #x807FFFFF 64) (bv #x81000000 64)))
		(check-true (not (pmp-check m (bv #x00700001 64) (bv #x007FFFFF 64))))
		(check-true (pmp-check m (bv #x10700001 64) (bv #x107FFFFF 64)))
		(check-false (pmp-check m (bv #x00700001 64) (bv #x107FFFFF 64)))
		(check-true (equal? (machine-mode m) 0)))
	(test-case "pmp-napot-settings"
		; test pmp_decode_cfg
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
		(check-equal? (list-ref setting5 3) 0)))

; Assert that kernel memory is equal between two machines
; (define (assert-kernel-mem-equal m1 m2)
; 	(define m1-ram (machine-ram m1))
; 	(define m(machine-ram m2))
; 	(vector-ref (machine-ram m) i)
; 	)
; (provide machine)

(define-test-suite kernel
	(test-case "kernel test"
		(define program (file->bytearray "kernel/kernel.bin"))
		(printf "~n* Running kernel.bin test ~n")
		(define ramsize 1000000)
		(define m (init-machine program ramsize))
		(execute-until-mret m)
		(print-pmp m)
		(check-true (equal? (machine-mode m) 0))))

(define (deep-copy-machine m)
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
		; (machine-ram m)
		(for/vector ([i (machine-ram m)])
			i)
		(machine-mode m)))

(define-test-suite noninterference
	(test-case "noninterference"
		(printf "~n* Running noninterference proof ~n")

		(define reset_program (file->bytearray "kernel/kernel.bin"))
		; set up our machine state
		(define ramsize 1000000)
		(define m (init-machine reset_program ramsize))
		(define m1 (deep-copy-machine m))

		; (define next_instr1 (step m)) ; step!

		; show that they can execute independently, but
		; still refer to the same symbolic variables.
		(print-csr m)
		(print-memory m #x80000 #x80010)
		(execute-until-mret m)
		(print-csr m)
		(print-memory m #x80000 #x80010)

		(print-csr m1)
		(print-memory m1 #x80000 #x80010)
		(execute-until-mret m1)
		(print-csr m1)
		(print-memory m1 #x80000 #x80010)
		(check-true #t)))

; (define res-instruction-check (run-tests instruction-check))
; (define res-utils (run-tests utils))
; (define res-high-level-test (run-tests high-level-test))
; (define res-kernel (run-tests kernel))
(define res-noninterference (run-tests noninterference))
