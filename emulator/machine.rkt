#lang rosette/safe

(require
  "pmp.rkt"
  "parameters.rkt")
(require (only-in racket/base for for/list in-range))
(require syntax/parse/define)

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))
(provide fresh-symbolic)

;; Structs to Build the Machine

; 31 64-bit-vectors (x0 is not an actual gpr)
(struct cpu
  (csrs gprs pc) #:mutable #:transparent)
(provide (struct-out cpu))

; control status registers for u and m mode
(struct csrs
  (mtvec mepc mstatus pmp)
  #:mutable #:transparent)
(provide csrs)

; cpu, ram, and mode (1 is machine, 0 is user)
(struct machine
  (cpu ram mode) #:mutable #:transparent)
(provide (struct-out machine))

;; Wrappers for Mutator and Accessor Functions

; be careful to decrement by 1 to access right location for gprs

(define (get-csr m csr)
  (cond
    [(eq? csr 'mtvec)     (csrs-mtvec     (cpu-csrs (machine-cpu m)))]
    [(eq? csr 'mepc)      (csrs-mepc      (cpu-csrs (machine-cpu m)))]
    [(eq? csr 'mstatus)   (csrs-mstatus   (cpu-csrs (machine-cpu m)))]
    [(eq? csr 'pmpcfg0)   (pmp-pmpcfg0 (csrs-pmp   (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpcfg2)   (pmp-pmpcfg2 (csrs-pmp   (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr0)  (pmp-pmpaddr0 (csrs-pmp  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr1)  (pmp-pmpaddr1 (csrs-pmp  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr2)  (pmp-pmpaddr2 (csrs-pmp  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr3)  (pmp-pmpaddr3 (csrs-pmp  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr4)  (pmp-pmpaddr4 (csrs-pmp  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr5)  (pmp-pmpaddr5 (csrs-pmp  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr6)  (pmp-pmpaddr6 (csrs-pmp  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr7)  (pmp-pmpaddr7 (csrs-pmp  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr8)  (pmp-pmpaddr8 (csrs-pmp  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr9)  (pmp-pmpaddr9 (csrs-pmp  (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr10) (pmp-pmpaddr10 (csrs-pmp (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr11) (pmp-pmpaddr11 (csrs-pmp (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr12) (pmp-pmpaddr12 (csrs-pmp (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr13) (pmp-pmpaddr13 (csrs-pmp (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr14) (pmp-pmpaddr14 (csrs-pmp (cpu-csrs (machine-cpu m))))]
    [(eq? csr 'pmpaddr15) (pmp-pmpaddr15 (csrs-pmp (cpu-csrs (machine-cpu m))))]
    [else
      ; (printf "No such CSR: ~a~n" csr)
      (illegal-instr m)]))
(provide get-csr)

(define (set-csr! m csr val)
  (define v_csr null)
  (cond 
    [(eq? csr 'mtvec)     (set-csrs-mtvec!     (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'mepc)      (set-csrs-mepc!      (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'mstatus)   (set-csrs-mstatus!   (cpu-csrs (machine-cpu m)) val)]
    ; [(eq? csr 'pmpcfg0)   (set-pmp-pmpcfg0! (csrs-pmp   (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpcfg2)   (set-pmp-pmpcfg2! (csrs-pmp   (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr0)  (set-pmp-pmpaddr0! (csrs-pmp  (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr1)  (set-pmp-pmpaddr1! (csrs-pmp  (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr2)  (set-pmp-pmpaddr2! (csrs-pmp  (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr3)  (set-pmp-pmpaddr3! (csrs-pmp  (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr4)  (set-pmp-pmpaddr4! (csrs-pmp  (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr5)  (set-pmp-pmpaddr5! (csrs-pmp  (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr6)  (set-pmp-pmpaddr6! (csrs-pmp  (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr7)  (set-pmp-pmpaddr7! (csrs-pmp  (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr8)  (set-pmp-pmpaddr8! (csrs-pmp  (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr9)  (set-pmp-pmpaddr9! (csrs-pmp  (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr10) (set-pmp-pmpaddr10! (csrs-pmp (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr11) (set-pmp-pmpaddr11! (csrs-pmp (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr12) (set-pmp-pmpaddr12! (csrs-pmp (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr13) (set-pmp-pmpaddr13! (csrs-pmp (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr14) (set-pmp-pmpaddr14! (csrs-pmp (cpu-csrs (machine-cpu m)) val))]
    ; [(eq? csr 'pmpaddr15) (set-pmp-pmpaddr15! (csrs-pmp (cpu-csrs (machine-cpu m)) val))]
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
  (machine-ram-read m pc 4))
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

; mem: uf, addr: bitvector ramsize-log2, value: bitvector 8
(define (uf-memory-write mem addr value)
  (lambda (addr*)
    (if (bveq addr addr*)
      value
      (mem addr*))))
(provide uf-memory-write)

; m: machine, addr, bitvector ramsize-log2, value: bitvector 8
(define (vector-memory-write! m addr value)
  (vector-set! (machine-ram m) (bitvector->natural addr) value))
(provide vector-memory-write!)

; mem: uf, addr: bitvector: ramsize-log2, val: bitvector 8
(define (memory-read mem addr)
  (if (use-fnmem)
    (mem addr)
    (vector-ref mem (bitvector->natural addr))))
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
      (define adj_addr (extract (sub1 (ramsize-log2)) 0 (bvadd addr (bv i 64))))
      (memory-read ba adj_addr)))
  ; little endian
  (apply concat (reverse bytes)))

(define (machine-ram-write! m addr value nbits)
  (define saddr (bvadd addr (base-address)))
  ; adjust to include the endpoint
  (define eaddr (bvadd addr (bv (sub1 (/ nbits 8)) 64) (base-address)))
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
    ; adjust pos for bitvector size (ramsize-log2)
    (define adj_pos (extract (sub1 (ramsize-log2)) 0 pos))
    (if (use-fnmem)
      (set-machine-ram! m (uf-memory-write (machine-ram m) adj_pos v))
      (vector-memory-write! m adj_pos v))))
