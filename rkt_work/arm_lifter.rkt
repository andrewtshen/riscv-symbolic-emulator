#lang rosette

(require syntax/parse/define)

(struct cpu (pc regs mem mpu) #:mutable #:transparent)
(provide (struct-out cpu))

(struct AP (kern user)) ; access permissions
(provide (struct-out AP))
(struct mpu_unit (start end RNR AP)) ; maybe can hard code in this information? RNR = region number, AP = access permissions
(provide (struct-out mpu_unit))

; backbone of the interpretor
(define (interpret c program)
  (define insn (fetch c program))
  (displayln c)
  (printf "insn: ~a~n" insn)
  (match insn
    [(list opcode rd rs rn imm)
     (execute c opcode rd rs rn imm)
     (when (not (equal? opcode 'ret))
       (interpret c program))]))
(provide interpret)

; fetch the program counter to make sure it's in bounds
(define (fetch c program)
  (define pc (cpu-pc c))
  (if (< pc 0) (displayln "failed") (void))
  (if (>= pc (vector-length program)) (displayln "failed") (void))
  (vector-ref program pc))

; helper functions to set/read registers
(define (cpu-reg c rs)
  (vector-ref (cpu-regs c) rs))
(provide cpu-reg)

(define (set-cpu-reg! c rd v)
  (vector-set! (cpu-regs c) rd v))

; execute each individual instruction
(define (execute c opcode rd rs rn imm)
  (define pc (cpu-pc c))
  (case opcode
    [(ret)
     (set-cpu-pc! c 0)]
    [(li)
     (set-cpu-pc! c (+ 1 pc))
     (set-cpu-reg! c rd imm)]
    [(snez)
     (set-cpu-pc! c (+ 1 pc))
     (if (= (cpu-reg c rs) 0)
         (set-cpu-reg! c rd 0)
         (set-cpu-reg! c rd 1))]
    [(mov)
     (set-cpu-pc! c (+ 1 pc))
     (set-cpu-reg! c rd (cpu-reg c rs))]
    [(bltz)
     (if (< (cpu-reg c rs) 0)
         (set-cpu-pc! c imm)
         (set-cpu-pc! c (+ 1 pc)))]
    [(add)
     (set-cpu-pc! (+ 1 pc))
     (set-cpu-reg! c rd (+ (cpu-reg rs) (cpu-reg rn)))]
    ))