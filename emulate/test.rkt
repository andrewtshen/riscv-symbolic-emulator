#lang rosette/safe

(require
	"load.rkt"
	"emulate.rkt"
	"execute.rkt"
	"machine.rkt")
(require (only-in racket/base for for/list in-range in-vector))
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
		(test-and-execute m)

		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))

		(define model-add (verify (begin 
			(assert (bveq (bvadd (list-ref gprsx 5)
														(list-ref gprsx 7))
										(list-ref gprsx 6))))))
		(check-true (unsat? model-add)))
	(test-case "sd/ld test"
		(define program (file->bytearray "build/store_and_load.bin"))
		(printf "~n* Running store_and_load.bin test ~n")
		; make machine
		(define ramsize 1000)
		(define m (init-machine program ramsize))
		(test-and-execute m)

		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))

		(define model-load-store (verify (begin 
			(assert (bveq (list-ref gprsx 2) (list-ref gprsx 3))))))

		(check-true (unsat? model-load-store)))
	(test-case "addi test"
		(define program (file->bytearray "build/addi.bin"))
		(define ramsize 1000)
		(printf "~n* Running addi.bin test ~n")
		(define m (init-machine program ramsize))
		(test-and-execute m)
		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		(define model-addi (verify (begin 
			(assert (bveq (list-ref gprsx 6) (bvadd (list-ref gprsx 5) (bv 32 64)))))))

		(check-true (unsat? model-addi)))
	(test-case "addw test"
		(define program (file->bytearray "build/addw.bin"))
		(define ramsize 1000)
		(printf "~n* Running addw.bin test ~n")
		(define m (init-machine program ramsize))
		(test-and-execute m)
		(define gprsx
			(for/list ([i (in-range 10 18)])
				(gprs-get-x m i)))
		(define model-addw (verify (begin 
			(assert (bveq (list-ref gprsx 6) (bvadd (list-ref gprsx 5) (list-ref gprsx 3)))))))

		(check-true (unsat? model-addw)))
	(test-case "sub test"
		(define program (file->bytearray "build/sub.bin"))
		(define ramsize 1000)
		(printf "~n* Running sub.bin test ~n")
		(define m (init-machine program ramsize))
		(test-and-execute m))
	; (test-case "jal test"
	; 	(define program (file->bytearray "build/jal.bin"))
	; 	(define ramsize 1000000)
	; 	(printf "~n* Running jal.bin test ~n")
	; 	(define m (init-machine program ramsize))
	; 	(test-and-execute m))
	; 	; (define gprsx
	; 	; 	(for/list ([i (in-range 10 18)])
	; 	; 		(gprs-get-x m i)))
	)
  

(define-test-suite high-level-test
	(test-case "stack test"
  	(define program (file->bytearray "build/stack.bin"))
  	(printf "~n* Running stack.bin test ~n" )
  	(define ramsize 1000)
  	(define m (init-machine program ramsize))
  	(test-and-execute m)))

(run-tests instruction-check)
(run-tests high-level-test)
