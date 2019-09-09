#lang rosette
(require syntax/parse/define)

(require "arm_lifter.rkt")

(define-simple-macro (build-symbols n:expr m:id)
  (build-vector n (lambda (i) (define-symbolic* m integer?) m)))

; the program we are testing
(define program (vector
                 '(snez 1 0 #f #f)
                 '(bltz #f 0 #f 4)
                 '(mov 0 1 #f #f)
                 '(ret #f #f #f #f)
                 '(li 0 #f #f -1)
                 '(ret #f #f #f #f)))

(struct state (a0 a1) #:mutable #:transparent)

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

(define-symbolic* r0 r1 integer?)
(define mpu_settings (mpu_unit 0 5 1 (AP 1 1)))
(define cpu-state
  (cpu 0
       (vector r0 r1)
       (build-symbols 10 ustack)
       (build-symbols 10 umem)
       mpu_settings)) ; both have full access

; Display our symbolic CPU
(displayln (cpu-regs cpu-state))
(displayln (cpu-pc cpu-state))
;(displayln (cpu-mem cpu-state))

(define-symbolic* R0 R1 integer?)
(define spec-state (state R0 R1))

; abstraction function: impl. cpu state to spec. state
(define (AF c)
  (state (cpu-reg c 0) (cpu-reg c 1)))
(solve (begin
          (assert (equal? (AF cpu-state) spec-state))
          (interpret cpu-state program) ; after program is run, cpu-state is essentially cpu-state'
          (define spec-state1 (spec-sign spec-state))
          (assert (not (equal? (AF cpu-state) spec-state1)))
          (displayln (asserts))))
; unsat, no solution that does not satisfy the spec state

; (asserts)
; (list (! (&& (= (ite (< X 0) -1 (ite (= 0 X) 0 1)) (ite (< 0 X1) 1 (ite (< X1 0) -1 0))) ; check that the R0 are equal
;             (|| (&& (= 0 X) (= 0 X1)) (&& (! (= 0 X)) (! (= 0 X1)))))) ; check that the R1 are equal
;      (&& (= X X1) (= Y Y1))) ; check that the initial states are equal