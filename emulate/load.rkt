#lang rosette/safe

(require (only-in racket/file file->bytes)
         (only-in racket/base bytes-length for/list in-range subbytes bytes-ref))

(require syntax/parse/define)
(require (only-in racket/base build-vector))

(define-simple-macro (make-sym-vector n:expr m:id)
    (build-vector n (lambda (i) (define-symbolic* m integer?) m)))

; example sym vector for bitvectors
; (make-sym-vector 3 foo)

; 31 64-bit-vectors (x0 isn't an actual gpr)
(struct cpu
    (gprs pc) #:mutable #:transparent)

; cpu and ram
(struct machine
    (cpu ram) #:mutable #:transparent)

; Wrappers for Accessor Functions
; be careful to decrement by 1 to access right location
(define (gprs-get-x m idx)
    (if (positive? idx)
        (vector-ref (cpu-gprs (machine-cpu m)) (- idx 1))
        (bv 0 64)))
    
(provide gprs-get-x)

(define (gprs-set-x! m idx val)
    (cond [(zero? idx)
            (printf "set zero vector invalid~n~n~n")])
    (vector-set! (cpu-gprs (machine-cpu m)) (- idx 1) val))
(provide gprs-set-x!)

(define (ram-get-x m idx)
    (vector-ref (machine-ram m) idx))
(provide ram-get-x)

(define (get-pc m)
    (cpu-pc (machine-cpu m)))
(provide get-pc)

(define (set-pc! m val)
    (set-cpu-pc! (machine-cpu m) val))
(provide set-pc!)

(define (file->bitvectors filename)
    (define contents (file->bytes filename))
    (define length (bytes-length contents))
    (assert (equal? (modulo length 4) 0))
    (list->vector
    (for/list ([i (in-range 0 (/ length 4))])
        (define b0 (bv (bytes-ref contents (+ 0 (* 4 i))) 8))
        (define b1 (bv (bytes-ref contents (+ 1 (* 4 i))) 8))
        (define b2 (bv (bytes-ref contents (+ 2 (* 4 i))) 8))
        (define b3 (bv (bytes-ref contents (+ 3 (* 4 i))) 8))
        (concat b3 b2 b1 b0))))
(provide file->bitvectors)

; get program example
; (define program (file->bitvectors "sum.bin"))
; (printf "Program: ~a~n" program)

(define (make-vector len bvec)
    (list->vector
        (for/list ([i (in-range 0 len)])
            bvec)))

; 32 64-bit gprs in the CPU and 32 bit-vectors for RAM
(define (init-machine program ramsize)
    (define proglength (vector-length program))
    (assert (>= ramsize proglength))
    (machine
        (cpu (make-vector 31 (bv 0 64)) 0) ; be careful of -1 for offset
        (vector-append
            program
            (make-vector (- ramsize proglength) (bv 0 32)))))
(provide init-machine)

; machine init example
; (define ramsize 10)
; (define m (init-machine program ramsize))
; (printf "~a~n" (machine-ram m))
; (displayln (gprs-get-x m 2))

(define base_address #x80000000)