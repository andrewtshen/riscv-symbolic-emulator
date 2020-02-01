#lang rosette/safe

(require (only-in racket/base error for/list in-range for))

; 31 64-bit-vectors (x0 is not an actual gpr)
(struct cpu
	(csrs gprs pc) #:mutable #:transparent)
(provide cpu)

; control status registers for u and m mode
(struct csrs
	(mtvec mepc mstatus pmpcfg0 pmpcfg2 pmpaddr0 pmpaddr1 pmpaddr2
		pmpaddr3 pmpaddr4 pmpaddr5 pmpaddr6 pmpaddr7 pmpaddr8 pmpaddr9
		pmpaddr10 pmpaddr11 pmpaddr12 pmpaddr13 pmpaddr14 pmpaddr15)
	#:mutable #:transparent)
(provide csrs)

; cpu, ram, and mode (1 is machine, 0 is user)
(struct machine
	(cpu ram mode) #:mutable #:transparent)
(provide (struct-out machine))

; Wrappers for Mutator and Accessor Functions
; be careful to decrement by 1 to access right location for gprs

(define (get-csr m csr)
	(define v_csr null)
	(cond
		[(equal? csr "mtvec") 		(set! v_csr (csrs-mtvec     (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mepc") 			(set! v_csr (csrs-mepc      (cpu-csrs (machine-cpu m))))]
		[(equal? csr "mstatus") 	(set! v_csr (csrs-mstatus   (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpcfg0") 	(set! v_csr (csrs-pmpcfg0   (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpcfg2") 	(set! v_csr (csrs-pmpcfg2   (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr0") 	(set! v_csr (csrs-pmpaddr0  (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr1") 	(set! v_csr (csrs-pmpaddr1  (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr2") 	(set! v_csr (csrs-pmpaddr2  (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr3") 	(set! v_csr (csrs-pmpaddr3  (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr4") 	(set! v_csr (csrs-pmpaddr4  (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr5") 	(set! v_csr (csrs-pmpaddr5  (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr6") 	(set! v_csr (csrs-pmpaddr6  (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr7") 	(set! v_csr (csrs-pmpaddr7  (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr8") 	(set! v_csr (csrs-pmpaddr8  (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr9") 	(set! v_csr (csrs-pmpaddr9  (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr10")	(set! v_csr (csrs-pmpaddr10 (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr11")	(set! v_csr (csrs-pmpaddr11 (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr12")	(set! v_csr (csrs-pmpaddr12 (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr13")	(set! v_csr (csrs-pmpaddr13 (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr14")	(set! v_csr (csrs-pmpaddr14 (cpu-csrs (machine-cpu m))))]
		[(equal? csr "pmpaddr15")	(set! v_csr (csrs-pmpaddr15 (cpu-csrs (machine-cpu m))))]
		[else (error "No CSR value found")])
	v_csr)
(provide get-csr)

(define (set-csr! m csr val)
	(define v_csr null)
	(cond 
		[(equal? csr "mtvec") 		(set-csrs-mtvec!     (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mepc") 			(set-csrs-mepc!      (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "mstatus") 	(set-csrs-mstatus!   (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpcfg0") 	(set-csrs-pmpcfg0!   (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpcfg2") 	(set-csrs-pmpcfg2!   (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr0") 	(set-csrs-pmpaddr0!  (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr1") 	(set-csrs-pmpaddr1!  (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr2") 	(set-csrs-pmpaddr2!  (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr3") 	(set-csrs-pmpaddr3!  (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr4") 	(set-csrs-pmpaddr4!  (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr5") 	(set-csrs-pmpaddr5!  (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr6") 	(set-csrs-pmpaddr6!  (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr7") 	(set-csrs-pmpaddr7!  (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr8") 	(set-csrs-pmpaddr8!  (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr9") 	(set-csrs-pmpaddr9!  (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr10")	(set-csrs-pmpaddr10! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr11")	(set-csrs-pmpaddr11! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr12")	(set-csrs-pmpaddr12! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr13")	(set-csrs-pmpaddr13! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr14")	(set-csrs-pmpaddr14! (cpu-csrs (machine-cpu m)) val)]
		[(equal? csr "pmpaddr15")	(set-csrs-pmpaddr15! (cpu-csrs (machine-cpu m)) val)]
		[else (error "No CSR value found")])
	v_csr)
(provide set-csr!)

(define (gprs-get-x m idx)
	(if (positive? idx)
		(vector-ref (cpu-gprs (machine-cpu m)) (- idx 1))
		(bv 0 64)))
(provide gprs-get-x)

(define (gprs-set-x! m idx val)
	(cond [(zero? idx)
			(error "set zero vector invalid")])
	(vector-set! (cpu-gprs (machine-cpu m)) (- idx 1) val))
(provide gprs-set-x!)

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

(define base_address #x80000000)
(provide base_address)