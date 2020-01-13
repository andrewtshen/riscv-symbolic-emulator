#lang rosette/safe

; Initialize the machine and load the program into the machine 
; to run symbolically. Initalize mutator and accessor functions 
; for changing the memory in the function.

(require (only-in racket/file file->bytes)
		 (only-in racket/base bytes-length for/list in-range subbytes bytes-ref error))


(require syntax/parse/define)
(require (only-in racket/base build-vector))

(define-simple-macro (make-sym-vector n:expr size:expr m:id)
	(build-vector n (lambda (i) (define-symbolic* m (bitvector size)) m)))

; 31 64-bit-vectors (x0 isn't an actual gpr)
(struct cpu
	(gprs pc) #:mutable #:transparent)

; cpu and ram
(struct machine
	(cpu ram) #:mutable #:transparent)

; Wrappers for Mutator and Accessor Functions
; be careful to decrement by 1 to access right location for gprs

; get gprs at index idx
(define (gprs-get-x m idx)
	(if (positive? idx)
		(vector-ref (cpu-gprs (machine-cpu m)) (- idx 1))
		(bv 0 64)))
(provide gprs-get-x)

; set gprs at index idx to value val
(define (gprs-set-x! m idx val)
	(cond [(zero? idx)
			(error "set zero vector invalid~n~n~n")])
	(vector-set! (cpu-gprs (machine-cpu m)) (- idx 1) val))
(provide gprs-set-x!)

; get machine ram
(provide machine-ram)

; get program counter
(define (get-pc m)
	(cpu-pc (machine-cpu m)))
(provide get-pc)

; set program counter
(define (set-pc! m val)
	(set-cpu-pc! (machine-cpu m) val))
(provide set-pc!)

; get next instruction using current program counter
(define (get-next-instr m)
	(define pc (get-pc m))
	(bytearray-read (machine-ram m) pc 4))
(provide get-next-instr)

; read an nbytes from a bytearray ba starting at address addr
(define (bytearray-read ba addr nbytes)
  (define bytes
    (for/list ([pos (in-range addr (+ addr nbytes))])
      (vector-ref ba pos)))
  ; little endian
  (apply concat (reverse bytes)))
(provide bytearray-read)

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
	(assert (>= ramsize proglength))
	(machine
		(cpu (make-sym-vector 31 64 gpr) 0) ; be careful of -1 for offset
		(vector-append
			program
			(make-sym-vector (- ramsize proglength) 8 mem))))
(provide init-machine)

(define base_address #x80000000)
(provide base_address)

; get program example
; (define program (file->bytearray "sum.bin"))
; (printf "Program: ~a~n" program)

; machine init example
; (define ramsize 100)
; (define m (init-machine program ramsize))
; (printf "~a~n" (machine-ram m))
; (displayln (gprs-get-x m 2))
; (get-next-instr m)

; example sym vector for bitvectors 3 bitvectors size 32 named foo
; (make-sym-vector 3 32 foo) 
