#lang rosette/safe

(require
  "pmp.rkt"
  "parameters.rkt"
  "concrete-optimizations.rkt"
  "csrs.rkt")
(require (only-in racket/base
                  for for/list in-range values))
(require syntax/parse/define)


(define-simple-macro (fresh-symbolic name type)
                     (let () (define-symbolic* name type) name))
(provide fresh-symbolic)

;; Structs to Build the Machine

; 31 64-bit-vectors (x0 is not an actual gpr)
(struct cpu
  (csrs gprs pc pmp) #:mutable #:transparent)
(provide (struct-out cpu))

; ; control status registers for u and m mode
; (struct csrs
;   (mtvec mepc mstatus pmp)
;   #:mutable #:transparent)
; (provide (struct-out csrs))

; cpu, ram, and mode (1 is machine, 0 is user)
(struct machine
  (cpu ram mode) #:mutable #:transparent)
(provide (struct-out machine))

;; General Accessors/Mutators

(define (machine-pmp m)
  (cpu-pmp (machine-cpu m)))
(provide machine-pmp)

(define (machine-csrs m)
  (cpu-csrs (machine-cpu m)))

(define (machine-gprs m)
  (cpu-gprs (machine-cpu m)))
(provide machine-gprs)

;; gprs Accessors/Mutators

; gprs: vector size 32, i: bitvector 5
(define (get-gprs-i gprs i)
  (cond
    [(bvzero? i) (bv 0 64)]
    [else (vector-ref-bv gprs i)]))
(provide get-gprs-i)

; gprs: vector size 32, i: bitvector 5, val: bitvector 64
(define (set-gprs-i! gprs i val)
  (cond
    [(bvzero? i) null]
    [else
     (vector-set!-bv gprs i val)]))
(provide set-gprs-i!)

;; csrs Accessors/Mutators

(define (machine-csr m csr)
  (cond
    [(bveq csr PMPCFG0)   (pmpcfg-value  (pmp-pmpcfgi  (machine-pmp m) 0))]
    [(bveq csr PMPCFG2)   (pmpcfg-value  (pmp-pmpcfgi  (machine-pmp m) 1))]
    [(bveq csr PMPADDR0)  (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 0))]
    [(bveq csr PMPADDR1)  (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 1))]
    [(bveq csr PMPADDR2)  (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 2))]
    [(bveq csr PMPADDR3)  (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 3))]
    [(bveq csr PMPADDR4)  (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 4))]
    [(bveq csr PMPADDR5)  (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 5))]
    [(bveq csr PMPADDR6)  (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 6))]
    [(bveq csr PMPADDR7)  (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 7))]
    [(bveq csr PMPADDR8)  (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 8))]
    [(bveq csr PMPADDR9)  (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 9))]
    [(bveq csr PMPADDR10) (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 10))]
    [(bveq csr PMPADDR11) (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 11))]
    [(bveq csr PMPADDR12) (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 12))]
    [(bveq csr PMPADDR13) (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 13))]
    [(bveq csr PMPADDR14) (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 14))]
    [(bveq csr PMPADDR15) (pmpaddr-value (pmp-pmpaddri (machine-pmp m) 15))]
    [else
     (get-csr (cpu-csrs (machine-cpu m)) csr)]))
(provide machine-csr)

(define (set-machine-csr! m csr val)
  (cond
    [(bveq csr PMPCFG0)   (set-pmpcfgi!  (machine-pmp m) 0  val)]
    [(bveq csr PMPCFG2)   (set-pmpcfgi!  (machine-pmp m) 1  val)]
    [(bveq csr PMPADDR0)  (set-pmpaddri! (machine-pmp m) 0  val)]
    [(bveq csr PMPADDR1)  (set-pmpaddri! (machine-pmp m) 1  val)]
    [(bveq csr PMPADDR2)  (set-pmpaddri! (machine-pmp m) 2  val)]
    [(bveq csr PMPADDR3)  (set-pmpaddri! (machine-pmp m) 3  val)]
    [(bveq csr PMPADDR4)  (set-pmpaddri! (machine-pmp m) 4  val)]
    [(bveq csr PMPADDR5)  (set-pmpaddri! (machine-pmp m) 5  val)]
    [(bveq csr PMPADDR6)  (set-pmpaddri! (machine-pmp m) 6  val)]
    [(bveq csr PMPADDR7)  (set-pmpaddri! (machine-pmp m) 7  val)]
    [(bveq csr PMPADDR8)  (set-pmpaddri! (machine-pmp m) 8  val)]
    [(bveq csr PMPADDR9)  (set-pmpaddri! (machine-pmp m) 9  val)]
    [(bveq csr PMPADDR10) (set-pmpaddri! (machine-pmp m) 10 val)]
    [(bveq csr PMPADDR11) (set-pmpaddri! (machine-pmp m) 11 val)]
    [(bveq csr PMPADDR12) (set-pmpaddri! (machine-pmp m) 12 val)]
    [(bveq csr PMPADDR13) (set-pmpaddri! (machine-pmp m) 13 val)]
    [(bveq csr PMPADDR14) (set-pmpaddri! (machine-pmp m) 14 val)]
    [(bveq csr PMPADDR15) (set-pmpaddri! (machine-pmp m) 15 val)]
    [else 
     (set-csr! (cpu-csrs (machine-cpu m)) csr val)]))
(provide set-machine-csr!)

; Get program counter
(define (machine-pc m)
  (cpu-pc (machine-cpu m)))
(provide machine-pc)

; Set program counter
(define (set-machine-pc! m val)
  (set-cpu-pc! (machine-cpu m) val))
(provide set-machine-pc!)

;; Illegal Instruction Handling

; Set up state for illegal instruction and return null to signal end of exec
(define (illegal-instr m)
  (set-machine-pc! m (bvsub (machine-csr m MTVEC) (base-address)))
  (set-machine-mode! m (bv 1 3)))
(provide illegal-instr)

;; Memory Reads/Writes

; mem: uf, addr: bitvector ramsize-log2, value: bitvector 8
(define (uf-memory-write mem addr value)
  (lambda (addr*)
    (if (bveq addr addr*)
        value
        (mem addr*))))
(provide uf-memory-write)

; m: machine, addr: bitvector ramsize-log2, value: bitvector 8
(define (vector-memory-write! m addr value)
  (vector-set!-bv (machine-ram m) addr value))
(provide vector-memory-write!)

; mem: uf, addr: bitvector: ramsize-log2, val: bitvector 8
(define (memory-read mem addr)
  (if (use-fnmem)
      (mem addr)
      (vector-ref-bv mem addr)))
(provide memory-read)

; Read an nbytes from a machine-ram ba starting at address addr
(define (machine-ram-read m addr nbytes)
  (define saddr (bvadd addr (base-address)))
  ; nbytes is always concrete so it is okay to use (bv x 64) here
  (define eaddr (bvadd addr (bv (* nbytes 8) 64) (base-address)))
  (define legal
    (if (use-concrete-optimizations)
        (concrete-pmp-check (machine-pmp m) (machine-mode m) saddr eaddr)
        (pmp-check (machine-pmp m) (machine-mode m) saddr eaddr)))
  
  (if legal
      (cond
        [(use-sym-optimizations) (fresh-symbolic val (bitvector (* nbytes 8)))]
        [(use-concrete-optimizations) (concrete-bytearray-read (machine-ram m) addr nbytes)]
        [else (bytearray-read (machine-ram m) addr nbytes)])
      'illegal-instruction))
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
  (define legal
    (if (use-concrete-optimizations)
        (concrete-pmp-check (machine-pmp m) (machine-mode m) saddr eaddr)
        (pmp-check (machine-pmp m) (machine-mode m) saddr eaddr)))
  
  ; machine mode (1) or legal, we can read the memory
  (when legal
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
