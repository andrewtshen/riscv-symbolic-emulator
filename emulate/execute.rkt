#lang rosette/safe

(require "load.rkt")

; execute each individual instruction symbolically
; and update the program count to the proper place.

(define (execute instr m)
    (define opcode (list-ref instr 0))
    (define pc (get-pc m))
    (cond
        [(equal? opcode "mret")
            (set-pc! m 0)]
        [(equal? opcode "addi")
            (define rd (bitvector->natural (list-ref instr 1)))
            (define rs1 (gprs-get-x m (bitvector->natural (list-ref instr 2))))
            (define imm (zero-extend (list-ref instr 3) (bitvector 64)))
            (gprs-set-x! m rd (bvadd rs1 imm))
            (set-pc! m (+ 1 pc))]
        [(equal? opcode "add")
            (define rd (bitvector->natural (list-ref instr 1)))
            (define rs1 (gprs-get-x m (bitvector->natural (list-ref instr 2))))
            (define rs2 (gprs-get-x m (bitvector->natural (list-ref instr 3))))

            (gprs-set-x! m rd (bvadd rs1 rs2))
            (set-pc! m (+ 1 pc))]
    ))
(provide execute)
