#lang rosette/safe

(require
	"machine.rkt")
(require (only-in racket/file file->bytes)
		 (only-in racket/base bytes-length for for/list in-range subbytes bytes-ref error))
(require syntax/parse/define)
(require (only-in racket/base build-vector))

; Initialize the machine and load the program into the machine to run symbolically. 
; Initalize mutator and accessor functions for changing the memory in the function.

; make-sym-vector creates a vector of symbolic bitvectors
(define-simple-macro (make-sym-vector n:expr size:expr m:id)
	(build-vector n (lambda (i) (define-symbolic* m (bitvector size)) m)))
	; (build-vector n (lambda (i) (bv 0 size)))) ; for testing with qemu

; convert a file to a bytearray
(define (file->bytearray filename)
	(define contents (file->bytes filename))
	(define length (bytes-length contents))
	(assert (equal? (modulo length 4) 0))
	(list->vector
	(for/list ([i (in-range 0 length)])
		(bv (bytes-ref contents i) 8))))
(provide file->bytearray)

; 32 64-bit gprs in the CPU and 32 bit-vectors for RAM
(define (init-machine program ramsize)
	(define proglength (vector-length program))
	(printf "ramsize: ~a~n" ramsize)
	(printf "proglength: ~a~n" proglength)
	(assert (>= ramsize proglength))
	(define-symbolic* mtvec mepc mstatus pmpcfg0 pmpcfg2 pmpaddr0 pmpaddr1 pmpaddr2
		pmpaddr3 pmpaddr4 pmpaddr5 pmpaddr6 pmpaddr7 pmpaddr8 pmpaddr9 pmpaddr10
		pmpaddr11 pmpaddr12 pmpaddr13 pmpaddr14 pmpaddr15 (bitvector 64))

	; set all the initial csrs to 0 (TODO: change to actual values)
	(set! mtvec (bv 0 64))
	(set! mepc (bv 0 64))
	(set! mstatus (bv 0 64))
	(set! pmpcfg0 (bv 0 64))
	(set! pmpcfg2 (bv 0 64))
	(set! pmpaddr0 (bv 0 64))
	(set! pmpaddr1 (bv 0 64))
	(set! pmpaddr2 (bv 0 64))
	(set! pmpaddr3 (bv 0 64))
	(set! pmpaddr4 (bv 0 64))
	(set! pmpaddr5 (bv 0 64))
	(set! pmpaddr6 (bv 0 64))
	(set! pmpaddr7 (bv 0 64))
	(set! pmpaddr8 (bv 0 64))
	(set! pmpaddr9 (bv 0 64))
	(set! pmpaddr10 (bv 0 64))
	(set! pmpaddr11 (bv 0 64))
	(set! pmpaddr12 (bv 0 64))
	(set! pmpaddr13 (bv 0 64))
	(set! pmpaddr14 (bv 0 64))
	(set! pmpaddr15 (bv 0 64))

	(define m
		(machine
			(cpu 
				(csrs
					mtvec mepc mstatus pmpcfg0 pmpcfg2 pmpaddr0 pmpaddr1 pmpaddr2
					pmpaddr3 pmpaddr4 pmpaddr5 pmpaddr6 pmpaddr7 pmpaddr8 pmpaddr9
					pmpaddr10 pmpaddr11 pmpaddr12 pmpaddr13 pmpaddr14 pmpaddr15)
				(make-sym-vector 31 64 gpr)
				0) ; be careful of -1 for offset
			(vector-append
				program
				(make-sym-vector (- ramsize proglength) 8 mem))
			1)) ; start in machine mode
	; default all gprs to 0
	(for [(i (in-range 1 32))]
		(gprs-set-x! m i (bv 0 64)))
	; do some special virt machine set up
	(gprs-set-x! m 5 (bv #x80000000 64))
	(gprs-set-x! m 10 (bv 1020 64))
	m)
(provide init-machine)

; ; get program example
; (define program (file->bytearray "sum.bin"))
; (printf "Program: ~a~n" program)

; ; machine init example
; (define ramsize 100)
; (define m (init-machine program ramsize))
; (printf "~a~n" (machine-ram m))
; (displayln (gprs-get-x m 2))
; (get-next-instr m)

; ; example sym vector for bitvectors 3 bitvectors size 32 named foo
; (make-sym-vector 3 32 foo) 
