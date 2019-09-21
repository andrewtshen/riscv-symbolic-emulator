#lang rosette
(require syntax/parse/define)

(require "arm_lifter.rkt")

(define-simple-macro (build-mem n:expr m:id)
  (build-vector n (lambda (i) (define-symbolic* m (bitvector 1)) m)))

; the program we are testing
(define program (vector
                 `(cmp #f 0 0 #f #f #f)
                 `(ble #f #f #f 4 #f #f)
                 `(mov 1 #f 1 #f #f #f)
                 `(ret #f #f #f #f #f #f)
                 `(cmp #f 0 0 #f #f #f)
                 `(bge #f #f #f 8 #f #f)
                 `(mov 1 #f -1 #f #f #f)
                 `(ret #f #f #f #f #f #f)
                 `(mov 1 #f 0 #f #f #f)
                 `(ret #f #f #f #f #f #f)
                 ))

(struct state (a0) #:mutable #:transparent)

; functional specification for the sign code
(define (spec-sign s)
  (define a0 (state-a0 s))
  (define sign (cond
                 [(positive? a0) 1]
                 [(negative? a0) -1]
                 [else 0]))
  ; (define scratch (if (zero? a0) 0 1)) this is intermediate register
  (state sign))

(define-symbolic* cpsr R0 R1 integer?)
(define cpu-state
  (cpu 0
       (vector R0 R1) ; test with this first then use (build-mem 16 umem) for full memory
       0 ; control program status register
       )) ; both have full access

(displayln cpu-state)

(interpret cpu-state program)
(displayln cpu-state)

(define-symbolic* r0 integer?)
(define spec-state (state r0))

; abstraction function: impl. cpu state to spec. state
#|(define (AF c)
  (state (cpu-reg c 1)))
(verify
 #:assume
 (assert (equal? (AF cpu-state) spec-state))
 #:guarantee
 (begin
   (interpret cpu-state program) ; after program is run, cpu-state is essentially cpu-state'
   (define spec-state1 (spec-sign spec-state))
   (assert (equal? (AF cpu-state) spec-state1))
   (displayln (asserts))))
; unsat, no solution that does not satisfy the spec state
|#

; (asserts)
;((= mem$0
;    (ite (< 0 r0$0) 1 (ite (< r0$0 0) -1 0))
;    ))

; (list (! (&& (= (ite (< X 0) -1 (ite (= 0 X) 0 1)) (ite (< 0 X1) 1 (ite (< X1 0) -1 0))) ; check that the R0 are equal
;             (|| (&& (= 0 X) (= 0 X1)) (&& (! (= 0 X)) (! (= 0 X1)))))) ; check that the R1 are equal
;      (&& (= X X1) (= Y Y1))) ; check that the initial states are equal
