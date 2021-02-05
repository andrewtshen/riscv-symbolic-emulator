#lang rosette/safe

(require
  "init.rkt"
  "emulate.rkt"
  "execute.rkt"
  "machine.rkt"
  "pmp.rkt"
  "decode.rkt"
  "parameters.rkt"
  "print_utils.rkt")
(require (only-in racket/base 
                  parameterize call-with-parameterization
                  parameterize* for for/list for/vector in-range for*))
(require profile)

; ; (define (test m size)
; ;   ; (printf "* Running boot sequence test ~n")
; ;   ; (parameterize
; ;   ;    ([use-sym-optimizations #f]
; ;   ;     [use-debug-mode #f]
; ;   ;     [use-fnmem #f]
; ;   ;     [use-concrete-mem #t])
; ;   ;  (execute-until-mret m))

; ;   ; (printf "Symbolics: ~a~n" (symbolics m))
; ;   ; (printf "Asserts: ~a~n" (symbolics m))
; ;   (for ([i (in-range size)])
; ;     (parameterize
; ;       ([use-fnmem #f])
; ;     ; (machine-ram-read m (bv #x0 64) 4)
; ;     (pmp-check m (bv #x80000000 64) (bv #x80000020 64))
; ;     ))
; ;   (printf "Size: ~a~n" size)
; ;   null)

; ; (define program (file->bytearray "kernel/kernel.bin"))
; ; (define m
; ;  (time (parameterize
; ;           ([use-sym-optimizations #f]
; ;            [use-debug-mode #f]
; ;            [use-fnmem #f]
; ;            [use-concrete-mem #t])
; ;         (init-machine-with-prog program))))

; ; (profile-thunk (lambda () (test m #x20000)))
; ; (time (test m #x20000))
; ; (define res (time (test m #x20000)))


; ; (define val
; ;   (parameterize
; ;     ([use-fnmem #f])
; ;   (machine-ram-read m (bv #x0 64) 4)))

; ; (printf "val: ~a~n" val)

; ; 00074683

; (define (test-loop1 size)
;   (define total 0)
;   (for ([i (in-range size)])
;     (let loop ([j 0])
;       (if (equal? j 16)
;          #t
;         (begin
;           (set! total (add1 total))
;           (loop (add1 j))))))
;   total)

; ; (profile-thunk (lambda () (test-loop #x20000)))
; (printf "Current PMP-Check method:~n")
; (time (test-loop1 #x20000))

; (define (test-loop2 size)
;   (define total 0)
;   (for ([i (in-range size)])
;     (for ([i (in-range 16)])
;       (set! total (add1 total))))
;   total)

; ; (profile-thunk (lambda () (test-loop2 #x20000)))
; (printf "Nested For Loop method:~n")
; (time (test-loop2 #x20000))

; (define (test-loop3 size)
;   (let loop ([i 0] [total 0])
;     (if (equal? i size)
;       total
;       (let inner-loop ([j 0])
;         (if (equal? j 16)
;           (loop (add1 i) (+ 16 total))
;           (inner-loop (add1 j)))))))

; ; (profile-thunk (lambda () (test-loop3 #x20000)))
; (printf "Nested Let Loop method:~n")
; (time (test-loop3 #x20000))

; (define (test-loop4 size)
;   (define total 0)
;   (for ([i (in-range size)])
;     (set! total (add1 total)))
;   total)

; ; (profile-thunk (lambda () (test-loop4 #x200000)))
; (printf "Flattened method:~n")
; (time (test-loop4 #x200000))

; (define (test-loop5 size)
;   (let loop ([i size] [total 0])
;     (if (not (zero? i))
;       (loop (sub1 i) (add1 total))
;       total)))

; ; (profile-thunk (lambda () (test-loop5 #x200000)))
; (printf "Descending flattened method:~n")
; (time (test-loop5 #x200000))

; (define (test-loop6 size)
;   (define total 0)
;   (for* ([i (in-range size)]
;          [j (in-range 16)])
;     (set! total (add1 total)))
;   total)

; ; (profile-thunk (lambda () (test-loop6 #x200000)))
; (printf "For* method:~n")
; (time (test-loop6 #x20000))

; (printf "-----------~n")
; (printf "Verify Total~n")
; (printf "~a~n" (test-loop1 #x20000))
; (printf "~a~n" (test-loop2 #x20000))
; (printf "~a~n" (test-loop3 #x20000))
; (printf "~a~n" (test-loop4 #x200000))
; (printf "~a~n" (test-loop5 #x200000))
; (printf "~a~n" (test-loop6 #x20000))
(printf "* Running boot sequence test ~n")
(define program (file->bytearray "kernel/kernel.bin"))
(define m
 (parameterize
     ([use-sym-optimizations #f]
      [use-debug-mode #f]
      [use-fnmem #f]
      [use-concrete-mem #t])
   (init-machine-with-prog program)))
(parameterize
   ([use-sym-optimizations #f]
    [use-debug-mode #f]
    [use-fnmem #f]
    [use-concrete-mem #t])
 (execute-until-mret m))

(printf "~a~n" (symbolics m))

 ; Check that after boot sequence machine mode is user mode (0) and in OK state
 (print-pmp m)
 ; (check-true (equal? (machine-mode m) 0))
 ; (assert-OK m)