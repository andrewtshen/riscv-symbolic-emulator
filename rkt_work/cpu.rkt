#lang rosette

(struct cpu (pc regs) #:mutable)

(define (interpret c program)
  (define insn (fetch c program))
  (printf "insn: ~a~n" insn)
  (match insn
    [(list opcode rd rs imm)
     (execute c opcode rd rs imm)
     (when (not (equal? opcode 'ret))
       (interpret c program))]))

(define (fetch c program)
  (define pc (cpu-pc c))
  (if (< pc 0) (display "failed\n") (display ""))
  (if (>= pc (vector-length program)) (display "failed\n") (display ""))
  (display "\n")
  (vector-ref program pc))

(define (cpu-reg c rs)
  (vector-ref (cpu-regs c) rs))

(define (set-cpu-reg! c rd v)
  (vector-set! (cpu-regs c) rd v))

(define (execute c opcode rd rs imm)
  (define pc (cpu-pc c))
  (case opcode
    [(ret)
     (set-cpu-pc! c 0)]
    [(li)
     (set-cpu-pc! c (+ 1 pc))]
    [(snez)
     (set-cpu-pc! c (+ 1 pc))
     (if (= (cpu-reg c rs) 0)
         (set-cpu-reg! c rd 0)
         (set-cpu-reg! c rd 1))]
    [(mv)
     (set-cpu-pc! c (+ 1 pc))]
    [(bltz)
     (if (< (cpu-reg c rs) 0)
         (set-cpu-pc! c imm)
         (set-cpu-pc! c (+ 1 pc)))]))

(define-symbolic X Y integer?)
(define c (cpu 0 (vector X Y)))
(define program (vector
                 '(snez 1 0 #f)
                 '(bltz #f 0 4)
                 '(mv 0 1 #f)
                 '(ret #f #f #f)
                 '(li 0 #f -1)
                 '(ret #f #f #f)))
(interpret c program)


(struct state (a0 a1)) ; specification state

; functional specification for the sign code
(define (spec-sign s)
  (define a0 (state-a0 s))
  (define sign (cond
                 [(positive? a0) 1]
                 [(negative? a0) -1]
                 [else 0]))
  (define scratch (if (zero? a0) 0 1))
  (state sign scratch))

; abstraction function: impl. cpu state to spec. state
(define (AF c)
  (state (cpu-reg c 0) (cpu-reg c 1)))


