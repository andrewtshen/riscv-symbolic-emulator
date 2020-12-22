#lang rosette/safe

(define (uf-memory-write! mem addr val)
  (lambda (addr*)
    (if (bveq addr addr*)
      val
      (mem addr*))))

(define (uf-memory-read mem addr)
  (mem addr))

(define (array-memory-write! mem addr value)
  (vector-set! mem addr value))

(define (array-memory-read mem addr)
  (vector-ref mem addr))

(require syntax/parse/define)
(require (only-in racket/base build-vector))

(define-simple-macro (make-sym-vector n:expr size:expr m:id)
  (build-vector n (lambda (i) (define-symbolic* m (bitvector size)) m)))

(define array_mem (make-sym-vector 4 8 mem))

(define-symbolic* pos idx (bitvector 2))
(define-symbolic* val (bitvector 8))

(printf "Before Write: ~v~n" (array-memory-read array_mem (bitvector->integer idx)))
(array-memory-write! array_mem (bitvector->integer pos) val)
(printf "After Write: ~v~n" (array-memory-read array_mem (bitvector->integer idx)))

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))

(define uf_mem (fresh-symbolic fnmem (~> (bitvector 2) (bitvector 8))))

(printf "Before Write: ~v~n" (uf-memory-read uf_mem idx))
(define uf_mem* (uf-memory-write! uf_mem pos val))
(printf "After Write: ~v~n" (uf-memory-read uf_mem* idx))
