#lang rosette/safe

(require (only-in racket/base for for/list for/vector in-range))
(require rackunit rackunit/text-ui)
(require syntax/parse/define)

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))


; mem: uf, addr: bitvector 32, val: bitvector 8
(define (memory-read mem addr)
	(mem addr))
(provide memory-read)

(define (memory-write-wrapper mem addr value)
	(if (and (bvsle (bv #x0 32) addr) (bvsle addr (bv #x2000 32)))
      (memory-write mem addr value)
      null))
(provide memory-write-wrapper)

(define (memory-write mem addr value)
  (lambda (addr*)
    (if (bveq addr addr*)
      value
      (mem addr*))))
(provide memory-write)

(define mem (fresh-symbolic mem (~> (bitvector 32) (bitvector 8))))

(define-symbolic* write-idx (bitvector 32))
(define-symbolic* write-val (bitvector 8))
; (define write-idx (bv #x20F 32))
; (define wrote-val (bv #x50 8))

(define mem1 (memory-write-wrapper mem write-idx write-val))

(printf "mem: ~a~n" mem)
(printf "read mem: ~a~n" (memory-read mem (bv #x20F 32)))
(printf "mem1: ~a~n" mem1)
(printf "read mem1: ~a~n" (memory-read mem1 (bv #x20F 32)))

(define-symbolic* sym-idx (bitvector 32))

; Currently PMP allows user to only write in the region 0x0 --> 0x1FF
(define model_noninterference_with_sym_idx (verify
	#:assume
	; sat cases like 0 <= sym-idx <= #x20000 work very quickly
	; unsat cases #x200 <= sym-idx <= #x200 that test small amounts of memory also run quickly
	; unsat cases #x200 <= sym-idx <= #x20000 that test large amounts of memory run very slowly (doesn't terminate)

	; use to test a range of values
	(assert (and (bvule (bv #x2001 32) sym-idx) (bvule sym-idx (bv #x4000 32))))
  ; (assert (and (bvule (bv #x2000 32) sym-idx) (bvule sym-idx (bv #x2000 32))))
	; ; use to test a certain value
	; (assert (bveq sym-idx (bv #x20F 32)))
	#:guarantee
	(assert (equal? (memory-read mem sym-idx)
					        (memory-read mem1 sym-idx)))))

(printf "Result: ~a~n" model_noninterference_with_sym_idx)