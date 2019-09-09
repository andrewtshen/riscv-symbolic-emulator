#lang rosette

(require "arm_lifter.rkt")

(define program (vector
                 '(snez 1 0 #f #f)
                 '(bltz #f 0 #f 4)
                 '(mov 0 1 #f #f)
                 '(ret #f #f #f #f)
                 '(li 0 #f #f -1)
                 '(ret #f #f #f #f)))

(struct state (a0 a1) #:mutable #:transparent) ; specification state

; functional specification for the sign code
(define (spec-sign s)
  (define a0 (state-a0 s))
  (define sign (cond
                 [(positive? a0) 1]
                 [(negative? a0) -1]
                 [else 0]))
  (define scratch (if (zero? a0) 0 1))
  (state sign scratch))

; test spec-impl relation

; lets us build a symbolic vector of length n
(define (build-symbols n)
    (build-vector n (lambda (i) (define-symbolic* m integer?) m)))

(define-symbolic X Y integer?)
(define cpu-state (cpu 0 (vector X Y)))

; Display our symbolic CPU
(displayln (cpu-regs cpu-state))
(displayln (cpu-pc cpu-state))
;(displayln (cpu-mem cpu-state))

;(interpret cpu-state program)

(define-symbolic X1 Y1 integer?)
(define spec-state (state X1 Y1))

; abstraction function: impl. cpu state to spec. state
(define (AF c)
  (state (cpu-reg c 0) (cpu-reg c 1)))

(assert (equal? (AF cpu-state) spec-state))
;(define cpu-state1 (interpret cpu-state program))
(interpret cpu-state program)
(define spec-state1 (spec-sign spec-state))
(assert (not (equal? (AF cpu-state) spec-state1)))
(asserts)

(solve #t)

;(list (= 10 X)
;      (&& (= X (ite (< 0 X1) 1 (ite (< X1 0) -1 0))) (|| (&& (= 0 X) (= 0 X1)) (&& (! (= 0 X)) (! (= 0 X1)))))
;      (&& (= X X1) (= Y Y1)))
