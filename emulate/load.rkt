#lang rosette/safe

; Initialize the machine and load the program into the machine 
; to run symbolically. Initalize mutator and accessor functions 
; for changing the memory in the function.

(require (only-in racket/file file->bytes)
		 (only-in racket/base bytes-length for for/list in-range subbytes bytes-ref error))
(require syntax/parse/define)
(require (only-in racket/base build-vector))

(define-simple-macro (make-sym-vector n:expr size:expr m:id)
	(build-vector n (lambda (i) (define-symbolic* m (bitvector size)) m)))

; 31 64-bit-vectors (x0 isn't an actual gpr)
(struct cpu
	(csrs gprs pc) #:mutable #:transparent)

(struct csrs
	(ustatus uie utvec uscratch uepc ucause ubadaddr uip 
	mstatus misa medeleg mideleg mie mtvec mscratch mepc
	mcause mbadaddr mip) 
	#:mutable #:transparent)

; cpu and ram
(struct machine
	(cpu ram) #:mutable #:transparent)

; Wrappers for Mutator and Accessor Functions
; be careful to decrement by 1 to access right location for gprs

(define (get-csr m csr)
	(define v_csr null)
	(cond 
		[(equal? csr "ustatus") 	(set! v_csr (csrs-ustatus (cpu-csrs (machine-cpu m))))]
		[(equal? csr "uie") 			(set! v_csr (csrs-uie (cpu-csrs (machine-cpu m))))]
		[(equal? csr "utvec") 		(set! v_csr (csrs-utvec (cpu-csrs (machine-cpu m))))]
		[(equal? csr "uscratch")	(set! v_csr (csrs-uscratch (cpu-csrs (machine-cpu m))))]
		[(equal? csr "uepc") 			(set! v_csr (csrs-uepc (cpu-csrs (machine-cpu m))))]
		[(equal? csr "ucause") 		(set! v_csr (csrs-ucause (cpu-csrs (machine-cpu m))))]
		[(equal? csr "ubadaddr")	(set! v_csr (csrs-ubadaddr (cpu-csrs (machine-cpu m))))]
		[(equal? csr "uip") 			(set! v_csr (csrs-uip (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mstatus") 	(set! v_csr (csrs-mstatus (cpu-csrs (machine-cpu m))))]
		[(equal? csr "misa") 			(set! v_csr (csrs-misa (cpu-csrs (machine-cpu m))))]
		[(equal? csr "medeleg") 	(set! v_csr (csrs-medeleg (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mideleg") 	(set! v_csr (csrs-mideleg (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mie") 			(set! v_csr (csrs-mie (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mtvec") 		(set! v_csr (csrs-mtvec (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mscratch")	(set! v_csr (csrs-mscratch (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mepc") 			(set! v_csr (csrs-mepc (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mcause") 		(set! v_csr (csrs-mcause (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mbadaddr")	(set! v_csr (csrs-mbadaddr (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mip") 			(set! v_csr (csrs-mip (cpu-csrs (machine-cpu m))))]
		[else (error "No CSR value found")])
	v_csr)
(provide get-csr)

(define (set-csr! m csr val)
	(define v_csr null)
	(cond 
		[(equal? csr "ustatus") 	(set-csrs-ustatus! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "uie") 			(set-csrs-uie! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "utvec") 		(set-csrs-utvec! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "uscratch")	(set-csrs-uscratch! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "uepc") 			(set-csrs-uepc! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "ucause") 		(set-csrs-ucause! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "ubadaddr")	(set-csrs-ubadaddr! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "uip") 			(set-csrs-uip! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mstatus") 	(set-csrs-mstatus! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "misa") 			(set-csrs-misa! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "medeleg") 	(set-csrs-medeleg! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mideleg") 	(set-csrs-mideleg! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mie") 			(set-csrs-mie! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mtvec") 		(set-csrs-mtvec! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mscratch")	(set-csrs-mscratch! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mepc") 			(set-csrs-mepc! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mcause") 		(set-csrs-mcause! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mbadaddr")	(set-csrs-mbadaddr! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mip") 			(set-csrs-mip! (cpu-csrs (machine-cpu m)) val)]
		[else (error "No CSR value found")])
	v_csr)
(provide set-csr!)

; get gprs at index idx
(define (gprs-get-x m idx)
	(if (positive? idx)
		(vector-ref (cpu-gprs (machine-cpu m)) (- idx 1))
		(bv 0 64)))
(provide gprs-get-x)

; set gprs at index idx to value val
(define (gprs-set-x! m idx val)
	(printf "idx ~a~n" idx)
	(cond [(zero? idx)
			(error "set zero vector invalid")])
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

(define (bytearray-write! ba addr value nbits)
  (when (not (equal? (modulo nbits 8) 0))
		(error "bytearray-write!: value has invalid width"))
  (define bytes (quotient nbits 8))
  (for ([i (in-range bytes)])
		; little-endian
		(let* ([pos (+ addr i)]
			[low (* 8 i)]
			[hi (+ 7 low)]
			[v (extract hi low value)])
		(vector-set! ba pos v))))
(provide bytearray-write!)

; convert a file to a bytearray
(define (file->bytearray filename)
	; (printf "filename: ~a~n " filename)
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
	(define-symbolic
		ustatus uie utvec uscratch uepc ucause ubadaddr uip 
		mstatus misa medeleg mideleg mie mtvec mscratch mepc
		mcause mbadaddr mip
		(bitvector 64))

	; set all the initial csr
	(set! ustatus (bv 0 64))
	(set! uie (bv 0 64))
	(set! utvec (bv 0 64))
	(set! uscratch (bv 0 64))
	(set! uepc (bv 0 64))
	(set! ucause (bv 0 64))
	(set! ubadaddr (bv 0 64))
	(set! uip  (bv 0 64))
	(set! mstatus (bv 0 64))
	(set! misa (bv 0 64))
	(set! medeleg (bv 0 64))
	(set! mideleg (bv 0 64))
	(set! mie (bv 0 64))
	(set! mtvec (bv 0 64))
	(set! mscratch (bv 0 64))
	(set! mepc (bv 0 64))
	(set! mcause (bv 0 64))
	(set! mbadaddr (bv 0 64))
	(set! mip (bv 0 64))

	(machine
		(cpu 
			(csrs 
				ustatus uie utvec uscratch uepc ucause ubadaddr uip 
				mstatus misa medeleg mideleg mie mtvec mscratch mepc
				mcause mbadaddr mip)
			(make-sym-vector 31 64 gpr)
			0) ; be careful of -1 for offset
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
