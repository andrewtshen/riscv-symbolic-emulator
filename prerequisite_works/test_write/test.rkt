#lang rosette/safe

(require (only-in racket/base for for/list for/vector in-range))
(require rackunit rackunit/text-ui)
(require syntax/parse/define)
(require "mem.rkt")

(define-simple-macro (fresh-symbolic name type)
  (let () (define-symbolic* name type) name))


(define-test-suite bound-check
  (test-case "bound check"
    (define mem (fresh-symbolic mem (~> (bitvector 32) (bitvector 8))))

  	(define-symbolic* write-idx (bitvector 32))
  	(define-symbolic* read-val (bitvector 32))
  	; (define write-idx (bv #x20F 32))
  	; (define read-val (bv #x50 32))

  	(define mem1 (memory-write-wrapper mem write-idx read-val))

  	(printf "mem: ~a~n" mem)
  	(printf "read mem: ~a~n" (memory-read mem (bv #x20F 32)))
  	(printf "mem1: ~a~n" mem1)
  	(printf "read mem1: ~a~n" (memory-read mem1 (bv #x20F 32)))

	

    (for ([i (in-range 0 #x2000)])
      ; (printf "testing: ~a~n" i)
      (define-symbolic* sym-idx (bitvector 32))
    	; Currently PMP allows user to only write in the region 0x0 --> 0x1FF
    	(define model_noninterference_with_sym_idx (verify
    		#:assume
    	  (assert (and (bvule (bv i 32) sym-idx) (bvule sym-idx (bv i 32))))
    		; ; use to test a certain value
    		; (assert (bveq sym-idx (bv #x20F 32)))
    		#:guarantee
    		(assert (equal? (memory-read mem sym-idx)
    						        (memory-read mem1 sym-idx)))))
      ; (printf "result: ~a~n" model_noninterference_with_sym_idx)
      )
  ))

(define res-bound-check (run-tests bound-check))