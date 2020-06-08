#lang rosette/safe

(require (only-in racket/base for for/list for/vector in-range))
(require rackunit rackunit/text-ui)
(require syntax/parse/define)

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))

; mem: uf, addr: bitvector 32, return: bitvector 8
(define (memory-read mem addr)
	(mem addr))
(provide memory-read)

; mimicking the pmp
(define (memory-write-wrapper mem addr value)
	(if (and (bvsle (bv #x0 32) addr) (bvsle addr (bv #x2000 32)))
      (memory-write mem addr value)
      null))
(provide memory-write-wrapper)

; mem: uf, addr: bitvector 32, value : bitvector 8
(define (memory-write mem addr value)
  (lambda (addr*)
    (if (bveq addr addr*)
      value
      (mem addr*))))
(provide memory-write)

(define mem (fresh-symbolic mem (~> (bitvector 32) (bitvector 8))))

; symbolic index and value to write
(define-symbolic* write-idx (bitvector 32))
(define-symbolic* write-val (bitvector 8))

(define mem1 (memory-write-wrapper mem write-idx write-val))

(define-symbolic* sym-idx (bitvector 32))

; Currently PMP allows user to only write in the region 0x0 --> 0x2000 (inclusive)
(define model_noninterference_with_sym_idx (verify
	#:assume
	; This memory is unwritable and is should result in unsat result.
	(assert (and (bvule (bv #x2001 32) sym-idx) (bvule sym-idx (bv #x4000 32))))
	#:guarantee
	(assert (equal? (memory-read mem sym-idx)
					        (memory-read mem1 sym-idx)))))

(printf "Result: ~a~n" model_noninterference_with_sym_idx)