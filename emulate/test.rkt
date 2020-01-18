#lang rosette/safe

(require "load.rkt")
(require "emulate.rkt")
(require (only-in racket/base for for/list in-range in-vector))
(require rackunit rackunit/text-ui)

(define-test-suite arithmetic-check
  (test-case "sum test"
    (define program (file->bytearray "build/sum.bin"))
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
			(displayln model-add)
			(check-equal? (unsat? model-add) #t)))

(define-test-suite store-load-check
	(test-case "sd/ld test"
		(define program (file->bytearray "build/store_and_load.bin"))
			; make machine
			(define ramsize 1000)
			(define m (init-machine program ramsize))
			(test-and-execute m)

			(define gprsx
				(for/list ([i (in-range 10 18)])
					(gprs-get-x m i)))

			(define model-load-store (verify (begin 
				(assert (bveq (list-ref gprsx 2) (list-ref gprsx 3))))))

			(print-csr m)
			(check-equal? (unsat? model-load-store) #t)))


(run-tests arithmetic-check)
; (run-tests store-load-check)
