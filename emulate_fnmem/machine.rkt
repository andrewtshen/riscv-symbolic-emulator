#lang rosette/safe

(require
  "pmp.rkt"
  "parameters.rkt")
(require (only-in racket/base for for/list in-range))
(require syntax/parse/define)

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))
(provide fresh-symbolic)

;; Structs

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

;; Wrappers for Mutator and Accessor Functions

; be careful to decrement by 1 to access right location for gprs

(define (get-csr m csr)
  (define v_csr null)
  (cond
    [(eq? csr 'mtvec)     (set! v_csr (csrs-mtvec     (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'mepc)      (set! v_csr (csrs-mepc      (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'mstatus)   (set! v_csr (csrs-mstatus   (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpcfg0)   (set! v_csr (csrs-pmpcfg0   (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpcfg2)   (set! v_csr (csrs-pmpcfg2   (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr0)  (set! v_csr (csrs-pmpaddr0  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr1)  (set! v_csr (csrs-pmpaddr1  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr2)  (set! v_csr (csrs-pmpaddr2  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr3)  (set! v_csr (csrs-pmpaddr3  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr4)  (set! v_csr (csrs-pmpaddr4  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr5)  (set! v_csr (csrs-pmpaddr5  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr6)  (set! v_csr (csrs-pmpaddr6  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr7)  (set! v_csr (csrs-pmpaddr7  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr8)  (set! v_csr (csrs-pmpaddr8  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr9)  (set! v_csr (csrs-pmpaddr9  (cpu-csrs (machine-cpu m))))]
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
    [(eq? csr 'mtvec)     (set-csrs-mtvec!     (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'mepc)      (set-csrs-mepc!      (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'mstatus)   (set-csrs-mstatus!   (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpcfg0)   (set-csrs-pmpcfg0!   (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpcfg2)   (set-csrs-pmpcfg2!   (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr0)  (set-csrs-pmpaddr0!  (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr1)  (set-csrs-pmpaddr1!  (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr2)  (set-csrs-pmpaddr2!  (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr3)  (set-csrs-pmpaddr3!  (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr4)  (set-csrs-pmpaddr4!  (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr5)  (set-csrs-pmpaddr5!  (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr6)  (set-csrs-pmpaddr6!  (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr7)  (set-csrs-pmpaddr7!  (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr8)  (set-csrs-pmpaddr8!  (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr9)  (set-csrs-pmpaddr9!  (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr10) (set-csrs-pmpaddr10! (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr11) (set-csrs-pmpaddr11! (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr12) (set-csrs-pmpaddr12! (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr13) (set-csrs-pmpaddr13! (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr14) (set-csrs-pmpaddr14! (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpaddr15) (set-csrs-pmpaddr15! (cpu-csrs (machine-cpu m)) val)]
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
      #t]
    [(zero? idx) #t]
    [else
      (illegal-instr m)]))
(provide gprs-set-x!)

; Get program counter
(define (get-pc m)
  (cpu-pc (machine-cpu m)))
(provide get-pc)

; Set program counter
(define (set-pc! m val)
  (set-cpu-pc! (machine-cpu m) val))
(provide set-pc!)

; Get next instruction using current program counter
(define (get-next-instr m)
  (define pc (get-pc m))
  (when (use-debug-mode) (printf "pc: ~a~n" pc))
  (define val (machine-ram-read m pc 4))
  (when (use-debug-mode) (printf "val: ~a~n" val))
  val)
(provide get-next-instr)

;; Illegal Instruction Handling

; Set up state for illegal instruction and return null to signal end of exec
(define (illegal-instr m)
  (set-pc! m (bvsub (get-csr m 'mtvec) (base-address)))
  (set-machine-mode! m 1)
  ; stop execution of instruction
  null)
(provide illegal-instr)

;; Memory Reads/Writes

; mem: uf, addr: bitvector 64, value: any
(define (memory-write mem addr value)
  (lambda (addr*)
    (if (bveq addr addr*)
      value
      (mem addr*))))
(provide memory-write)

; mem: uf, addr: bitvector 64, val: bitvector 8
(define (memory-read mem addr)
  (mem addr))
(provide memory-read)

; Read an nbytes from a machine-ram ba starting at address addr
(define (machine-ram-read m addr nbytes)
  (define saddr (bvadd addr (base-address)))
  ; nbytes is always concrete so it is okay to use (bv x 64) here
  (define eaddr (bvadd addr (bv (* nbytes 8) 64) (base-address)))
  (define legal (pmp-check m saddr eaddr))

  (if (or (equal? (machine-mode m) 1) legal)
    (if (use-sym-optimizations)
      (fresh-symbolic val (bitvector (* nbytes 8)))
      (bytearray-read (machine-ram m) addr nbytes))
    null))

(provide machine-ram-read)

(define (bytearray-read ba addr nbytes)
  (define bytes
    (for/list ([i (in-range nbytes)])
      ; adjust address for bitvector size (ramsize-log2) and index
      (define adj_addr (extract (- (ramsize-log2) 1) 0 (bvadd addr (bv i 64))))
      (memory-read ba adj_addr)))
  ; little endian
  (apply concat (reverse bytes)))

(define (machine-ram-write! m addr value nbits)
  (define saddr (bvadd addr (base-address)))
  ; adjust to include the endpoint
  (define eaddr (bvadd addr (bv (- (/ nbits 8) 1) 64) (base-address)))
  (define legal (pmp-check m saddr eaddr))

  ; machine mode (1) or legal, we can read the memory
  (when (or (equal? (machine-mode m) 1) legal)
    (bytearray-write! m addr value nbits))

  legal)
(provide machine-ram-write!)

(define (bytearray-write! m addr value nbits)
  (define bytes (quotient nbits 8))
  (for ([i (in-range 0 bytes)])
    (define pos (bvadd addr (integer->bitvector i (bitvector 64))))
    (define v 
      (if (use-sym-optimizations)
        (fresh-symbolic v (bitvector 8))
        ; little-endian formatting
        (begin
          (define low (* 8 i))
          (define hi (+ 7 low))
          (define v (extract hi low value))
          v)))
    (when (use-debug-mode)
      (printf "pos: ~a and v: ~a~n" pos v))
    ; adjust pos for bitvector size (ramsize-log2)
    (define adj_pos (extract (- (ramsize-log2) 1) 0 pos))
    (set-machine-ram! m (memory-write (machine-ram m) adj_pos v))))

;; PMP checks

(define (pmpcfg-check m pmpcfg saddr eaddr pmpaddrs)
  (define legal null)
  (define done #f)
  ; somewhat hacky way of getting right regs, doesn't work if id odd
  ; but this is okay because for risc-V 64, always id even
  (for ([i (in-range 0 8)]
        [pmp_name pmpaddrs]
        #:break (equal? done #t))

    (define settings (pmp-decode-cfg pmpcfg i))

    ; TODO check type of access
    (define R (list-ref settings 0))
    (define W (list-ref settings 1))
    (define X (list-ref settings 2))
    (define A (list-ref settings 3))
    (when (use-debug-mode)
      (printf "R:~a W:~a X:~a A:~a~n" R W X A))
    (when (equal? A 1)
      (define pmp (get-csr m pmp_name))
      (define pmp_bounds (pmp-decode-napot pmp))

      (define pmp_start (list-ref pmp_bounds 0))
      (define pmp_end (bvadd (list-ref pmp_bounds 0) (list-ref pmp_bounds 1)))
      (when (use-debug-mode)
        (printf "pmp_start: ~a~n" pmp_start)
        (printf "pmp_end: ~a~n" pmp_end)
        (printf "saddr: ~a~n" saddr)
        (printf "eaddr: ~a~n" eaddr))

      ; test the proper bounds
      (define slegal (bv-between saddr pmp_start pmp_end))
      (define elegal (bv-between eaddr pmp_start pmp_end))

      (when (and slegal elegal)
        (set! done #t)
        (if (and (equal? R 1) (equal? W 1) (equal? X 1))
          (set! legal #t)
          (set! legal #f)))))
  legal)

; PMP test address ranging from saddr to eaddr 
(define (pmp-check m saddr eaddr)
  ; check pmpcfg0, iterate through each register
  (define pmpcfg0 (get-csr m 'pmpcfg0))
  (define pmpcfg0_regs (list 'pmpaddr0 'pmpaddr1 'pmpaddr2 'pmpaddr3
                             'pmpaddr4 'pmpaddr5 'pmpaddr6 'pmpaddr7))
  (define legal (pmpcfg-check m pmpcfg0 saddr eaddr pmpcfg0_regs))

  ; check pmpcfg2, iterate through each register
  (when (equal? legal null)
    (define pmpcfg2_regs (list 'pmpaddr8 'pmpaddr9 'pmpaddr10 'pmpaddr11
                               'pmpaddr12 'pmpaddr13 'pmpaddr14 'pmpaddr15))
    (define pmpcfg2 (get-csr m 'pmpcfg2))
    (set! legal (pmpcfg-check m pmpcfg2 saddr eaddr pmpcfg2_regs)))
  (if (equal? legal null)
    #t
    legal))
(provide pmp-check)
