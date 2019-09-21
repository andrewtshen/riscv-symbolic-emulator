#lang rosette

(require syntax/parse/define)

(struct cpu (pc regs mems cpsr) #:mutable #:transparent)
(provide (struct-out cpu))

; backbone of the interpretor
(define (interpret c program)
  (define insn (fetch c program))
  (displayln c)
  (printf "insn: ~a~n" insn)
  (match insn
    [(list opcode Rd Rn Op2 addr Rm Rs)
     (execute c opcode Rd Rn Op2 addr Rm Rs)
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

(define (cpu-mem c rs)
  (vector-ref (cpu-mems c) rs))
(provide cpu-mem)

(define (set-cpu-mem! c rd v)
  (vector-set! (cpu-mems c) rd v))

; Display our symbolic CPU
(define (display-cpu c)
  (display "---\ncpu: ")
  (display "registers: ")
  (displayln (cpu-regs c))
  (display "pc: ")
  (displayln (cpu-pc c))
  (display "mem: ")
  (displayln (cpu-mems c))
  (displayln "---"))
(provide display-cpu)

; execute each individual instruction
(define (execute c opcode Rd Rn Op2 addr Rm Rs)
  (define pc (cpu-pc c))
  (case opcode
    [(ret)
     (set-cpu-pc! c 0)]
    [(mov)
     (set-cpu-pc! c (+ 1 pc))
     (set-cpu-mem! c Rd Op2)]
    [(ble)
     (if (>= 0 (cpu-cpsr c))
         (set-cpu-pc! c addr)
         (set-cpu-pc! c (+ 1 pc)))]
    [(bge)
     (if (<= 0 (cpu-cpsr c))
         (set-cpu-pc! c addr)
         (set-cpu-pc! c (+ 1 pc)))]
    [(cmp)
     (set-cpu-pc! c (+ 1 pc))
     (set-cpu-cpsr! c (- (cpu-mem c Rn) Op2))
     (displayln (cpu-cpsr c))]
    ))