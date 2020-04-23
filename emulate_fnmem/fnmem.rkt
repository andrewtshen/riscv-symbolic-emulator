#lang rosette

(require (only-in racket/file file->bytes)
		 (only-in racket/base bytes-length for for/list in-range subbytes bytes-ref in-naturals))
(require syntax/parse/define)
(require (only-in racket/base build-vector))


(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))

(define (new-uf-memory nbits)
  (fresh-symbolic mem (~> (bitvector nbits) (bitvector 32))))

(define (memory-write mem addr value)
  (lambda (addr*)
    (if (equal? addr addr*)
      value
      (mem addr*))))

(define (memory-read mem value)
	(mem value))

(define mem-uf (new-uf-memory 32))

(set! mem-uf (memory-write mem-uf (bv #x2000 32) 20))
(printf "memory x2000: ~a~n" (memory-read mem-uf (bv #x2000 32)))
(printf "memory x0: ~a~n" (memory-read mem-uf (bv #x0 32)))
(printf "memory: ~a~n" mem-uf)

(define x (fresh-symbolic x (bitvector 32)))
(set! mem-uf (memory-write mem-uf x 20))
(printf "reading symbolic value: ~a~n" (memory-read mem-uf x))
(printf "memory: ~a~n" mem-uf)