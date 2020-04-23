#lang rosette/safe

(require
	"machine.rkt")
(require (only-in racket/file file->bytes)
		 (only-in racket/base bytes-length for for/list in-range subbytes bytes-ref in-naturals))
(require syntax/parse/define)
(require (only-in racket/base build-vector))

;; Details

; Initialize the machine and load the program into the machine to run symbolically. 
; Initalize mutator and accessor functions for changing the memory in the function.
; Memory is stored as uninterpreted functions which map 32 bit keys/addresses to 8 bit values.
; 32 64-bit gprs in the CPU and 32 bit-vectors for RAM

;; Macros

; Create a vector of symbolic variables
(define-simple-macro (make-sym-vector n:expr size:expr m:id)
	(build-vector n (lambda (i) (define-symbolic* m (bitvector size)) m)))
	; (build-vector n (lambda (i) (bv 0 size)))) ; set mem to 0 for testing with qemu

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))

;; Helper Methods

; Convert a file to a bytearray
(define (file->bytearray filename)
	(define contents (file->bytes filename))
	(define length (bytes-length contents))
	(unless (equal? (modulo length 4) 0)
		(printf "Cannot read incomplete files~n"))
	(list->vector
	(for/list ([i (in-range 0 length)])
		(bv (bytes-ref contents i) 8))))
(provide file->bytearray)

(define (print-memory m ramsize)
	(for [(i (in-range 0 ramsize))]
		(printf "i: ~a value: ~a~n" i ((machine-ram m) (bv i 32)))))

;; Different ways to set up machine

(define (init-machine-with-prog program ramsize)
	(define proglength (vector-length program))
	(printf "ramsize: ~a~n" ramsize)
	(printf "proglength: ~a~n" proglength)
	(unless (>= ramsize proglength)
		(printf "Not enough RAM provided to run program~n"))
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

	(define mem (fresh-symbolic mem (~> (bitvector 32) (bitvector 8))))

	(for ([byte program]
				[i (in-naturals)])
		(set! mem (memory-write mem (bv i 32) byte)))

	(define m
		(machine
			(cpu 
				(csrs
					mtvec mepc mstatus pmpcfg0 pmpcfg2 pmpaddr0 pmpaddr1 pmpaddr2
					pmpaddr3 pmpaddr4 pmpaddr5 pmpaddr6 pmpaddr7 pmpaddr8 pmpaddr9
					pmpaddr10 pmpaddr11 pmpaddr12 pmpaddr13 pmpaddr14 pmpaddr15)
				(make-sym-vector 31 64 gpr) ; be careful of -1 for offset
				(bv 0 64)) ; make pc symbolic
			mem
			1)) ; start in machine mode

	; default all gprs to 0
	(for [(i (in-range 1 32))]
		(gprs-set-x! m i (bv 0 64)))

	; do some special virt machine set up
	(gprs-set-x! m 5 (bv #x80000000 64))
	(gprs-set-x! m 10 (bv 1020 64))
	m)
(provide init-machine-with-prog)

(define (init-machine ramsize)
	(define-symbolic* mtvec mepc mstatus pmpcfg0 pmpcfg2 pmpaddr0 pmpaddr1 pmpaddr2
		pmpaddr3 pmpaddr4 pmpaddr5 pmpaddr6 pmpaddr7 pmpaddr8 pmpaddr9 pmpaddr10
		pmpaddr11 pmpaddr12 pmpaddr13 pmpaddr14 pmpaddr15 pc (bitvector 64))

	(set! mtvec (bv #x0000000080000000 64))
	(set! pmpcfg0 (bv #x000000000000001f 64))
	(set! pmpcfg2 (bv #x0000000000000018 64))
	(set! pmpaddr0 (bv #x0000000020000bff 64))
	(set! pmpaddr1 (bv 0 64))
	(set! pmpaddr1 (bv 0 64))
	(set! pmpaddr2 (bv 0 64))
	(set! pmpaddr3 (bv 0 64))
	(set! pmpaddr4 (bv 0 64))
	(set! pmpaddr5 (bv 0 64))
	(set! pmpaddr6 (bv 0 64))
	(set! pmpaddr7 (bv 0 64))
	(set! pmpaddr8 (bv #x7fffffffffffffff 64))
	(set! pmpaddr9 (bv 0 64))
	(set! pmpaddr10 (bv 0 64))
	(set! pmpaddr11 (bv 0 64))
	(set! pmpaddr12 (bv 0 64))
	(set! pmpaddr13 (bv 0 64))
	(set! pmpaddr14 (bv 0 64))
	(set! pmpaddr15 (bv 0 64))

	(define mem (fresh-symbolic mem (~> (bitvector 32) (bitvector 8))))

	(define m
		(machine
			(cpu 
				(csrs
					mtvec mepc mstatus pmpcfg0 pmpcfg2 pmpaddr0 pmpaddr1 pmpaddr2
					pmpaddr3 pmpaddr4 pmpaddr5 pmpaddr6 pmpaddr7 pmpaddr8 pmpaddr9
					pmpaddr10 pmpaddr11 pmpaddr12 pmpaddr13 pmpaddr14 pmpaddr15)
				(make-sym-vector 31 64 gpr) ; be careful of -1 for offset
				pc) ; make pc symbolic
			mem
			0)) ; start in user mode
	m)
(provide init-machine)

;; Examples

; ; get program and init machine example
; (define program (file->bytearray "build/add.bin"))
; (printf "Program: ~a~n" program)
; (define ramsize 100)
; (define m (init-machine-with-prog program ramsize))
; (print-memory m ramsize)
; ; ((machine-ram m) (bv 100 32)) ; illegal instruction
; (displayln (gprs-get-x m 2))
; (get-next-instr m)

; ; machine init examples
; (define ramsize 100)
; (define m1 (init-machine ramsize))
; (print-memory m1 ramsize)

; ; example symbolic vector: 3 bitvectors size 32 named foo$0 foo$1 foo$2
; (make-sym-vector 3 32 foo) 
