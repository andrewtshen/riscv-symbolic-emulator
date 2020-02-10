#lang rosette/safe

(require
	"pmp.rkt")
(require (only-in racket/base for for/list in-range))

; 31 64-bit-vectors (x0 is not an actual gpr)
(struct cpu
	(csrs gprs pc) #:mutable #:transparent)
(provide (struct-out cpu))

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
		[(eq? csr 'mtvec) 	  (set! v_csr (csrs-mtvec     (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'mepc) 			(set! v_csr (csrs-mepc      (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'mstatus)  	(set! v_csr (csrs-mstatus   (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpcfg0)  	(set! v_csr (csrs-pmpcfg0   (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpcfg2)  	(set! v_csr (csrs-pmpcfg2   (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr0) 	(set! v_csr (csrs-pmpaddr0  (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr1) 	(set! v_csr (csrs-pmpaddr1  (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr2) 	(set! v_csr (csrs-pmpaddr2  (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr3) 	(set! v_csr (csrs-pmpaddr3  (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr4) 	(set! v_csr (csrs-pmpaddr4  (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr5) 	(set! v_csr (csrs-pmpaddr5  (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr6) 	(set! v_csr (csrs-pmpaddr6  (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr7) 	(set! v_csr (csrs-pmpaddr7  (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr8) 	(set! v_csr (csrs-pmpaddr8  (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr9) 	(set! v_csr (csrs-pmpaddr9  (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr10) (set! v_csr (csrs-pmpaddr10 (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr11) (set! v_csr (csrs-pmpaddr11 (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr12) (set! v_csr (csrs-pmpaddr12 (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr13) (set! v_csr (csrs-pmpaddr13 (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr14) (set! v_csr (csrs-pmpaddr14 (cpu-csrs (machine-cpu m))))]
		[(eq? csr 'pmpaddr15) (set! v_csr (csrs-pmpaddr15 (cpu-csrs (machine-cpu m))))]
		[else
			; (printf "No such CSR: ~a~n" csr)
			(illegal-instr m)])
	v_csr)
(provide get-csr)

(define (set-csr! m csr val)
	(define v_csr null)
	(cond 
		[(eq? csr 'mtvec) 		(set-csrs-mtvec!     (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'mepc) 			(set-csrs-mepc!      (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'mstatus) 	(set-csrs-mstatus!   (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpcfg0) 	(set-csrs-pmpcfg0!   (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpcfg2) 	(set-csrs-pmpcfg2!   (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr0) 	(set-csrs-pmpaddr0!  (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr1) 	(set-csrs-pmpaddr1!  (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr2) 	(set-csrs-pmpaddr2!  (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr3) 	(set-csrs-pmpaddr3!  (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr4) 	(set-csrs-pmpaddr4!  (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr5) 	(set-csrs-pmpaddr5!  (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr6) 	(set-csrs-pmpaddr6!  (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr7) 	(set-csrs-pmpaddr7!  (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr8) 	(set-csrs-pmpaddr8!  (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr9) 	(set-csrs-pmpaddr9!  (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr10)	(set-csrs-pmpaddr10! (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr11)	(set-csrs-pmpaddr11! (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr12)	(set-csrs-pmpaddr12! (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr13)	(set-csrs-pmpaddr13! (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr14)	(set-csrs-pmpaddr14! (cpu-csrs (machine-cpu m)) val)]
		[(eq? csr 'pmpaddr15)	(set-csrs-pmpaddr15! (cpu-csrs (machine-cpu m)) val)]
		[else 
			; (printf "No such CSR: ~a~n" csr)
			(illegal-instr m)])
	v_csr)
(provide set-csr!)

(define (gprs-get-x m idx)
	(cond
		[(and (< 0 idx) (< idx 32))
			(vector-ref (cpu-gprs (machine-cpu m)) (- idx 1))]
		[(zero? idx)
			(bv 0 64)]
		[else
			(illegal-instr m)]))
(provide gprs-get-x)

(define (gprs-set-x! m idx val)
	(cond 
		[(and (< 0 idx) (< idx 32))
			(vector-set! (cpu-gprs (machine-cpu m)) (- idx 1) val)
			; TODO: replace with something else more informative
			#t]
		[(zero? idx)
			#t]
		[else
			(illegal-instr m)]))
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
	(printf "pc: ~a~n" pc)
	(define val (machine-ram-read m pc 4))
	(printf "val: ~a~n" val)
	val)
(provide get-next-instr)

; set up state for illegal instruction and return null to signal end of exec
(define (illegal-instr m)
	(set-pc! m (bvsub (get-csr m 'mtvec) base_address))
	(set-machine-mode! m 1)
	; stop execution of instruction
	null)
(provide illegal-instr)

; read an nbytes from a machine-ram ba starting at address addr
(define (machine-ram-read m addr nbytes)
	(define saddr (bvadd addr base_address))
	(define eaddr (bvadd addr (bv (* nbytes 8) 64) base_address))
	(define legal (pmp-check m saddr eaddr))

	; machine mode (1) or legal, we can read the memory
	(if (or (equal? (machine-mode m) 1) legal)
		(bytearray-read (machine-ram m) (bitvector->natural addr) nbytes)
		null))
(provide machine-ram-read)

(define (bytearray-read ba addr nbytes)
	(define bytes
		(for/list ([i (in-range nbytes)])
	  	(vector-ref ba (+ addr i))))
  ; little endian
  (apply concat (reverse bytes)))

(define (machine-ram-write! m addr value nbits)
	(define saddr (bvadd addr base_address))
	(define eaddr (bvadd addr (bv nbits 64) base_address))
	(define legal (pmp-check m saddr eaddr))

	; machine mode (1) or legal, we can read the memory
	(when (or (equal? (machine-mode m) 1) legal)
  	(bytearray-write! (machine-ram m) (bitvector->natural addr) value nbits))

	(if legal #t null))
(provide machine-ram-write!)

(define (bytearray-write! ba addr value nbits)
  (define bytes (quotient nbits 8))
  (for ([i (in-range bytes)])
		; little-endian
		(let* ([pos (+ addr i)]
			[low (* 8 i)]
			[hi (+ 7 low)]
			[v (extract hi low value)])
		(vector-set! ba pos v))))

(define base_address (bv #x80000000 64))
(provide base_address)

; PMP Checking Stuff

(define (pmpcfg-check m pmpcfg saddr eaddr pmpaddrs)
	(define legal #f)
	(define done #f)

	; somewhat hacky way of getting right regs, doesn't work if id odd
	(for ([i (in-range 0 8)]
				[pmp_name pmpaddrs]
				#:break (equal? done #t))
		(define settings (pmp-decode-cfg pmpcfg i))
		; TODO check type of access
		(define R (list-ref settings 0))
		(define W (list-ref settings 1))
		(define X (list-ref settings 2))
		(define A (list-ref settings 3))
		; (printf "~a ~a ~a ~a~n" R W X A)
		(cond [(equal? A 1)
			(define pmp (get-csr m pmp_name))
			(define pmp_bounds (pmp-decode-napot pmp))
			(define pmp_start (list-ref pmp_bounds 0))
			(define pmp_end (bvadd pmp_start (list-ref pmp_bounds 1)))

			(define slegal (bv-between saddr pmp_start pmp_end))
			(define elegal (bv-between eaddr pmp_start pmp_end))
			; if slegal #t and elegal #f, create illegal instruction
			; if elegal #f and slegal #t, create illegal instruction
			(cond
				[(and slegal (not elegal))
					(set! legal #f)]
				[(and elegal (not slegal))
					(set! legal #f)]
				[(and elegal slegal)
					(set! legal #t)
					(set! done #t)]
				[(and (not elegal) (not slegal))
					; Subcases
					; 1. both less than pmp_start -> continue testing other pmpcfgs
					; 2. both greater than pmp_start -> continue testing other pmpcfgs
					; 3. one below and one after -> illegal
					(cond
						[(and (bvult eaddr pmp_start) (bvult saddr pmp_end))
							null]
						[(and (bvult pmp_start eaddr) (bvult pmp_end saddr))
							null]
						[(and (bvult saddr pmp_start) (bvult pmp_end eaddr))
							(set! legal #f)
							(set! done #t)])]
				[else
					(illegal-instr m)])]))
	legal)

; test address ranging from saddr to eaddr 
(define (pmp-check m saddr eaddr)
	; check pmpcfg0, iterate through each register
	(define pmpcfg0 (get-csr m 'pmpcfg0))
	(define pmpcfg0_regs (list 'pmpaddr0 'pmpaddr1 'pmpaddr2 'pmpaddr3
														'pmpaddr4 'pmpaddr5 'pmpaddr6 'pmpaddr7))
	(define legal (pmpcfg-check m pmpcfg0 saddr eaddr pmpcfg0_regs))
	; check pmpcfg2, iterate through each register
	(when (not legal)
		(define pmpcfg2_regs (list 'pmpaddr8 'pmpaddr9 'pmpaddr10 'pmpaddr11
															'pmpaddr12 'pmpaddr13 'pmpaddr14 'pmpaddr15))
		(define pmpcfg2 (get-csr m 'pmpcfg2))
		(set! legal (pmpcfg-check m pmpcfg2 saddr eaddr pmpcfg2_regs)))
	legal)
(provide pmp-check)

(define (print-pmp m)
	(printf "pmpcfg0: ~a~n" (get-csr m 'pmpcfg0))
	(printf "pmpcfg2: ~a~n" (get-csr m 'pmpcfg2))
	(printf "pmpaddr0: ~a~n" (get-csr m 'pmpaddr0))
	(printf "pmpaddr1: ~a~n" (get-csr m 'pmpaddr1))
	(printf "pmpaddr2: ~a~n" (get-csr m 'pmpaddr2))
	(printf "pmpaddr3: ~a~n" (get-csr m 'pmpaddr3))
	(printf "pmpaddr4: ~a~n" (get-csr m 'pmpaddr4))
	(printf "pmpaddr5: ~a~n" (get-csr m 'pmpaddr5))
	(printf "pmpaddr6: ~a~n" (get-csr m 'pmpaddr6))
	(printf "pmpaddr7: ~a~n" (get-csr m 'pmpaddr7))
	(printf "pmpaddr8: ~a~n" (get-csr m 'pmpaddr8))
	(printf "pmpaddr0 base/range: ~a~n" (pmp-decode-napot (get-csr m 'pmpaddr0)))
	(printf "pmpaddr1 base/range: ~a~n" (pmp-decode-napot (get-csr m 'pmpaddr1)))
	(printf "pmpaddr8 base/range: ~a~n" (pmp-decode-napot (get-csr m 'pmpaddr8))))
(provide print-pmp)
