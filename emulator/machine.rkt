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
(provide (struct-out csrs))

; cpu, ram, and mode (1 is machine, 0 is user)
(struct machine
  (cpu ram mode) #:mutable #:transparent)
(provide (struct-out machine))

;; Wrappers for Mutator and Accessor Functions

; Helpers for accessing csrs from the machine
(define (get-pmpaddr-from-machine m i)
  (vector-ref (pmp-pmpaddrs (csrs-pmp (cpu-csrs (machine-cpu m)))) i))
(provide get-pmpaddr-from-machine)

(define (get-pmpcfg-from-machine m i)
  (vector-ref (pmp-pmpcfgs (csrs-pmp (cpu-csrs (machine-cpu m)))) i))
(provide get-pmpcfg-from-machine)

(define (get-csrs-from-machine m)
  (cpu-csrs (machine-cpu m)))

; Helpers for writing to pmp registers
(define (write-to-pmpaddr! m i val)
  ; Set the value for the pmp first
  (set-pmpaddr-value! (vector-ref (pmp-pmpaddrs (csrs-pmp (cpu-csrs (machine-cpu m)))) i) val)

  ; decode the value
  (define pmp_bounds (pmp-decode-napot val))
  (define pmp_start (list-ref pmp_bounds 0))
  (define pmp_end (bvadd (list-ref pmp_bounds 0) (list-ref pmp_bounds 1)))

  (set-pmpaddr-start_addr! (vector-ref (pmp-pmpaddrs (csrs-pmp (cpu-csrs (machine-cpu m)))) i) pmp_start)
  (set-pmpaddr-end_addr! (vector-ref (pmp-pmpaddrs (csrs-pmp (cpu-csrs (machine-cpu m)))) i) pmp_end))
(provide write-to-pmpaddr!)

(define (write-to-pmpcfg! m i val)
  (set-pmpcfg-value! (vector-ref (pmp-pmpcfgs (csrs-pmp (cpu-csrs (machine-cpu m)))) i) val)
  (for ([id (in-range 8)])
    (define settings (pmp-decode-cfg val id))
    (define old_settings (get-pmpcfg-setting (vector-ref (pmp-pmpcfgs (csrs-pmp (cpu-csrs (machine-cpu m)))) i) id))
    (when (and (bveq (pmpcfg_setting-A old_settings) (bv 0 2))
               (not (bveq (pmpcfg_setting-A settings) (bv 0 2))))
      (set-pmp-num_implemented!
        (csrs-pmp (cpu-csrs (machine-cpu m)))
        (add1 (get-pmp-num_implemented m))))
    (vector-set! (pmpcfg-settings (vector-ref (pmp-pmpcfgs (csrs-pmp (cpu-csrs (machine-cpu m)))) i)) id settings)))
(provide write-to-pmpcfg!)

; Get the value contained in a csr
; be careful to decrement by 1 to access right location for gprs
(define (get-csr m csr)
  (cond
    [(eq? csr 'mtvec)     (csrs-mtvec   (get-csrs-from-machine m))]
    [(eq? csr 'mepc)      (csrs-mepc    (get-csrs-from-machine m))]
    [(eq? csr 'mstatus)   (csrs-mstatus (get-csrs-from-machine m))]
    [(eq? csr 'pmpcfg0)   (pmpcfg-value (get-pmpcfg-from-machine m 0))]
    [(eq? csr 'pmpcfg2)   (pmpcfg-value (get-pmpcfg-from-machine m 1))]
    [(eq? csr 'pmpaddr0)  (pmpaddr-value (get-pmpaddr-from-machine m 0))]
    [(eq? csr 'pmpaddr1)  (pmpaddr-value (get-pmpaddr-from-machine m 1))]
    [(eq? csr 'pmpaddr2)  (pmpaddr-value (get-pmpaddr-from-machine m 2))]
    [(eq? csr 'pmpaddr3)  (pmpaddr-value (get-pmpaddr-from-machine m 3))]
    [(eq? csr 'pmpaddr4)  (pmpaddr-value (get-pmpaddr-from-machine m 4))]
    [(eq? csr 'pmpaddr5)  (pmpaddr-value (get-pmpaddr-from-machine m 5))]
    [(eq? csr 'pmpaddr6)  (pmpaddr-value (get-pmpaddr-from-machine m 6))]
    [(eq? csr 'pmpaddr7)  (pmpaddr-value (get-pmpaddr-from-machine m 7))]
    [(eq? csr 'pmpaddr8)  (pmpaddr-value (get-pmpaddr-from-machine m 8))]
    [(eq? csr 'pmpaddr9)  (pmpaddr-value (get-pmpaddr-from-machine m 9))]
    [(eq? csr 'pmpaddr10) (pmpaddr-value (get-pmpaddr-from-machine m 10))]
    [(eq? csr 'pmpaddr11) (pmpaddr-value (get-pmpaddr-from-machine m 11))]
    [(eq? csr 'pmpaddr12) (pmpaddr-value (get-pmpaddr-from-machine m 12))]
    [(eq? csr 'pmpaddr13) (pmpaddr-value (get-pmpaddr-from-machine m 13))]
    [(eq? csr 'pmpaddr14) (pmpaddr-value (get-pmpaddr-from-machine m 14))]
    [(eq? csr 'pmpaddr15) (pmpaddr-value (get-pmpaddr-from-machine m 15))]
    [else
      ; (printf "No such CSR: ~a~n" csr)
      (illegal-instr m)]))
(provide get-csr)

(define (set-csr! m csr val)
  (define v_csr null)
  (cond 
    [(eq? csr 'mtvec)     (set-csrs-mtvec!   (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'mepc)      (set-csrs-mepc!    (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'mstatus)   (set-csrs-mstatus! (cpu-csrs (machine-cpu m)) val)]
    [(eq? csr 'pmpcfg0)   (write-to-pmpcfg! m 0 val)]
    [(eq? csr 'pmpcfg2)   (write-to-pmpcfg! m 1 val)]
    [(eq? csr 'pmpaddr0)  (write-to-pmpaddr! m 0  val)]
    [(eq? csr 'pmpaddr1)  (write-to-pmpaddr! m 1  val)]
    [(eq? csr 'pmpaddr2)  (write-to-pmpaddr! m 2  val)]
    [(eq? csr 'pmpaddr3)  (write-to-pmpaddr! m 3  val)]
    [(eq? csr 'pmpaddr4)  (write-to-pmpaddr! m 4  val)]
    [(eq? csr 'pmpaddr5)  (write-to-pmpaddr! m 5  val)]
    [(eq? csr 'pmpaddr6)  (write-to-pmpaddr! m 6  val)]
    [(eq? csr 'pmpaddr7)  (write-to-pmpaddr! m 7  val)]
    [(eq? csr 'pmpaddr8)  (write-to-pmpaddr! m 8  val)]
    [(eq? csr 'pmpaddr9)  (write-to-pmpaddr! m 9  val)]
    [(eq? csr 'pmpaddr10) (write-to-pmpaddr! m 10 val)]
    [(eq? csr 'pmpaddr11) (write-to-pmpaddr! m 11 val)]
    [(eq? csr 'pmpaddr12) (write-to-pmpaddr! m 12 val)]
    [(eq? csr 'pmpaddr13) (write-to-pmpaddr! m 13 val)]
    [(eq? csr 'pmpaddr14) (write-to-pmpaddr! m 14 val)]
    [(eq? csr 'pmpaddr15) (write-to-pmpaddr! m 15 val)]
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

;; PMP Check

(define (get-pmp-num_implemented m)
  (pmp-num_implemented (csrs-pmp (cpu-csrs (machine-cpu m)))))
(provide get-pmp-num_implemented)

; PMP test address ranging from saddr to eaddr 
(define (pmp-check m saddr eaddr)
  (define pmpcfg0 (get-pmpcfg-from-machine m 0))
  (define pmpcfg2 (get-pmpcfg-from-machine m 1))
  ; Iterate through each pmpaddr and break at first matching
  (let loop ([i 0])
    (define settings 
      (if (< i 8)
        (get-pmpcfg-setting pmpcfg0 i)
        (get-pmpcfg-setting pmpcfg2 (- i 8))))
    ; (printf "setting: ~a~n" settings)
    ; (printf "saddr: ~a, eadder: ~a~n" saddr eaddr)
    (define R (pmpcfg_setting-R settings))
    (define W (pmpcfg_setting-W settings))
    (define X (pmpcfg_setting-X settings))
    (define A (pmpcfg_setting-A settings))
    (define L (pmpcfg_setting-L settings))

    ; For now we only implement A = 3 (NAPOT)
    (define bounds 
      (cond
        [(bveq A (bv 0 2))
          ; Unimplemented, so just return no access
          (list #f #f)]
        [(bveq A (bv 3 2))
          (define pmpaddr (get-pmpaddr-from-machine m i))
          (define pmp_start (pmpaddr-start_addr pmpaddr))
          (define pmp_end (pmpaddr-end_addr pmpaddr))
          ; (printf "start: ~a~n" pmp_start)
          ; (printf "end: ~a~n" pmp_end)

          ; Test the proper bounds, #t means allow access, #f means disallow access
          (define slegal (bv-between saddr pmp_start pmp_end))
          (define elegal (bv-between eaddr pmp_start pmp_end))
          (list slegal elegal)]
        [else
          (illegal-instr m)]))

    (define slegal (list-ref bounds 0))    
    (define elegal (list-ref bounds 1))
    ; (printf "~a ~a ~a ~a ~a ~a~n" pmp_start pmp_end saddr eaddr slegal elegal)

    ; Check saddr and eaddr match the pmpaddri range
    (if (and slegal elegal)
      ; Check if pmpaddri is locked
      ; TODO: Write an "pmpaddr-islocked" function for simplicity?
      (if (equal? L (bv 0 1))
        ; Check machine mode
        (cond
          [(equal? (machine-mode m) 1) #t]
          [(equal? (machine-mode m) 0)
            ; TODO: actually check what the access type is
            (and (bveq R (bv 1 1)) (bveq W (bv 1 1)) (bveq X (bv 1 1)))]
          [else
            ; TODO: implement other mode support (probably as simple as letting S and U be the same, see Docs)
            (illegal-instr m)])
        ; TODO: Implement locked variant of access, for now just return false (no access)
        #f)
      ; check if there are more pmpaddrs
      (if (equal? i 15)
        (equal? (get-pmp-num_implemented m) 0)
        (loop (add1 i))))))
(provide pmp-check)

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

