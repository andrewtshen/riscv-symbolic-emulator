#lang rosette/safe

(require
  "pmp.rkt"
  "machine.rkt"
  "parameters.rkt"
  "print_utils.rkt")
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

(define-simple-macro (make-pmpaddrs n:expr)
  (build-vector n (lambda (i) (define p (make-pmpaddr)) p)))

(define-simple-macro (make-pmpcfgs n:expr)
  (build-vector n (lambda (i) (define p (make-pmpcfg)) p)))

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

;; Different ways to set up machine

(define (init-machine-with-prog program)
  (define proglength (vector-length program))
  (define ramsize (expt 2 (ramsize-log2)))
  (unless (>= ramsize proglength)
    (printf "Not enough RAM provided to run program~n"))
  (define-symbolic* mtvec mepc mstatus (bitvector 64))

  (define pmps
    (pmp
      (make-pmpcfgs 2)
      (make-pmpaddrs 16)
      0))

  ; set all the initial csrs to 0 (TODO: change to actual values)
  (set! mtvec (bv 0 64))
  (set! mepc (bv 0 64))
  (set! mstatus (bv 0 64))

  ; use this for undefined memory
  (define fnmem (fresh-symbolic fnmem (~> (bitvector (ramsize-log2)) (bitvector 8))))
  ; use this for defined memory but it's "fake" code in the sense that it maps everything to 0
  ; (define fnmem (lambda (addr*) (bv 0 8)))

  ; All concrete values here, so we can use (bv i 64) without issues
  (when (use-fnmem)
    (for ([byte program]
          [i (in-naturals)])
      (set! fnmem (uf-memory-write fnmem (bv i (ramsize-log2)) byte))))

  (define m
    (machine
      (cpu 
        (csrs
          mtvec mepc mstatus pmps)
        (make-sym-vector 31 64 gpr) ; be careful of -1 for offset
        (bv 0 64)) ; set pc to 0 when loading with program
      (if (use-fnmem)
        fnmem
        (vector-append
          program
          (make-sym-vector (- (expt 2 (ramsize-log2)) proglength) 8 mem)))
      1)) ; start in machine mode
  ; default all gprs to 0
  (for [(i (in-range 1 32))]
    (gprs-set-x! m i (bv 0 64)))

  ; do some special virt machine set up
  (gprs-set-x! m 5 (bv #x80000000 64))
  (gprs-set-x! m 10 (bv 1020 64))

  ; set up values for pmp
  ; TODO: Check that these values are correct
  (write-to-pmpcfg! m 0 (bv 0 64))
  (write-to-pmpcfg! m 1 (bv 0 64))
  (write-to-pmpaddr! m 0 (bv 0 64))
  (write-to-pmpaddr! m 1 (bv 0 64))
  (write-to-pmpaddr! m 2 (bv 0 64))
  (write-to-pmpaddr! m 3 (bv 0 64))
  (write-to-pmpaddr! m 4 (bv 0 64))
  (write-to-pmpaddr! m 5 (bv 0 64))
  (write-to-pmpaddr! m 6 (bv 0 64))
  (write-to-pmpaddr! m 7 (bv 0 64))
  (write-to-pmpaddr! m 8 (bv 0 64))
  (write-to-pmpaddr! m 9 (bv 0 64))
  (write-to-pmpaddr! m 10 (bv 0 64))
  (write-to-pmpaddr! m 11 (bv 0 64))
  (write-to-pmpaddr! m 12 (bv 0 64))
  (write-to-pmpaddr! m 13 (bv 0 64))
  (write-to-pmpaddr! m 14 (bv 0 64))
  (write-to-pmpaddr! m 15 (bv 0 64))
  m)
(provide init-machine-with-prog)

(define (init-machine)
  (define-symbolic* mtvec mepc mstatus pc (bitvector 64))

  (define pmps
    (pmp
      (make-pmpcfgs 2)
      (make-pmpaddrs 16)
      0))

  (set! mtvec (bv #x0000000080000080 64))

  (define fnmem (fresh-symbolic fnmem (~> (bitvector (ramsize-log2)) (bitvector 8))))
  (define m
    (machine
      (cpu 
        (csrs
          mtvec mepc mstatus pmps)
        (make-sym-vector 31 64 gpr) ; be careful of -1 for offset
        pc) ; symbolic pc
      (if (use-fnmem)
        fnmem
        (make-sym-vector (expt 2 (ramsize-log2)) 8 mem))
      0)) ; start in user mode

  ; Write the PMP information
  (write-to-pmpcfg! m  0  (bv #x000000000000001f 64))
  (write-to-pmpcfg! m  1  (bv #x0000000000000018 64))
  (write-to-pmpaddr! m 0  (bv #x000000002000bfff 64))
  (write-to-pmpaddr! m 1  (bv 0 64))
  (write-to-pmpaddr! m 2  (bv 0 64))
  (write-to-pmpaddr! m 3  (bv 0 64))
  (write-to-pmpaddr! m 4  (bv 0 64))
  (write-to-pmpaddr! m 5  (bv 0 64))
  (write-to-pmpaddr! m 6  (bv 0 64))
  (write-to-pmpaddr! m 7  (bv 0 64))
  (write-to-pmpaddr! m 8  (bv #x7fffffffffffffff 64))
  (write-to-pmpaddr! m 9  (bv 0 64))
  (write-to-pmpaddr! m 10 (bv 0 64))
  (write-to-pmpaddr! m 11 (bv 0 64))
  (write-to-pmpaddr! m 12 (bv 0 64))
  (write-to-pmpaddr! m 13 (bv 0 64))
  (write-to-pmpaddr! m 14 (bv 0 64))
  (write-to-pmpaddr! m 15 (bv 0 64))

  m)
(provide init-machine)

;; Examples

; ; machine init example
; (define m (init-machine))
; (printf "~a~n" m)

; ; machine init with program example
; (define program (file->bytearray "kernel/kernel.bin"))
; (define m (init-machine-with-prog program))
; (printf "~a~n" m)

; ; example symbolic vector: 3 bitvectors size 32 named foo$0 foo$1 foo$2
; (make-sym-vector 3 32 foo) 
