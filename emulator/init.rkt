#lang rosette/safe

(require
  "pmp.rkt"
  "machine.rkt"
  "parameters.rkt"
  "print-utils.rkt"
  "csrs.rkt")
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
  (if (use-concrete-mem)
    (build-vector n (lambda (i) (bv 0 size)))
    (build-vector n (lambda (i) (define-symbolic* m (bitvector size)) m))))

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

;; Different ways to set up machine

(define (init-machine-with-prog program)
  (define proglength (vector-length program))
  (define ramsize (expt 2 (ramsize-log2)))
  (unless (>= ramsize proglength)
    (printf "Not enough RAM provided to run program~n"))

  ; Make and set all the initial csrs to 0 (TODO: change to actual values)
  (define csrs (make-csrs))
  (set-csr! csrs MTVEC (bv 0 64))
  (set-csr! csrs MEPC (bv 0 64))
  (set-csr! csrs MSTATUS (bv 0 64))
  (set-csr! csrs MHARTID (bv 0 64))
  (set-csr! csrs MIE (bv 0 64))
  ; TODO: Set MPRV to 0
  
  ; Set pc to 0 when loading with program
  (define pc (bv 0 64))
  
  ; Start in machine mode
  (define mode M_MODE)

  ; Set up PMP and default configurations
  (define pmp (make-pmp))
  ; TODO: Check that these values are correct (zeroing all pmp regs)
  (set-pmpcfgi! pmp 0 (bv 0 64))
  (set-pmpcfgi! pmp 1 (bv 0 64))
  (for ([i (in-range 16)])
    (set-pmpaddri! pmp i (bv 0 64)))

  ; Set up memory based on if using fnmem and load program
  (define fnmem (fresh-symbolic fnmem (~> (bitvector (ramsize-log2)) (bitvector 8))))
  (when (use-fnmem)
    (for ([byte program]
          [i (in-naturals)])
      (set! fnmem (uf-memory-write fnmem (bv i (ramsize-log2)) byte))))
  (define mem
    (if (use-fnmem)
       fnmem
       (vector-append program (make-sym-vector (- (expt 2 (ramsize-log2)) proglength) 8 mem))))

  ; Make gprs, default to zero, and do some special virt machine setup
  (define gprs (make-sym-vector 32 64 gpr))
  (for [(i (in-range 32))]
    (set-gprs-i! gprs (bv i 5) (bv 0 64)))

  (set-gprs-i! gprs (bv 10 5) (bv #x0 64))
  (set-gprs-i! gprs (bv 11 5) (bv 1020 64))

  (machine
   (cpu csrs gprs pc pmp)
   mem
   mode))
(provide init-machine-with-prog)

(define (init-machine)
  ; Use symbolic pc
  (define-symbolic* pc (bitvector 64))
  
  ; Set up csrs
  (define csrs (make-csrs))
  (set-csr! csrs MTVEC (bv #x0000000080000080 64))
  
  ; Start in user mode
  (define mode U_MODE) 

  ; Set up PMP and default configurations to enable ONLY #x0000000080020000 - #x000000000001ffff
  (define pmp (make-pmp))
  (for ([i (in-range 16)])
    (set-pmpaddri! pmp i (bv 0 64)))
  (set-pmpcfgi! pmp 0 (bv #x000000000000001f 64))
  (set-pmpcfgi! pmp 1 (bv #x0000000000000018 64))
  (set-pmpaddri! pmp 0 (bv #x000000002000bfff 64))
  (set-pmpaddri! pmp 8 (bv #x7fffffffffffffff 64))

  ; Set up memory based on if using fnmem
  (define fnmem (fresh-symbolic fnmem (~> (bitvector (ramsize-log2)) (bitvector 8))))
  (define mem
    (if (use-fnmem)
         fnmem
         (make-sym-vector (expt 2 (ramsize-log2)) 8 mem)))

  ; Make gprs
  (define gprs (make-sym-vector 32 64 gpr)) ; be careful of -1 for offset

  (machine
   (cpu csrs gprs pc pmp)
   mem
   mode))
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
