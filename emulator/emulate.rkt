#lang rosette/safe

(require
  "init.rkt"
  "execute.rkt"
  "machine.rkt"
  "pmp.rkt"
  "parameters.rkt"
  "print-utils.rkt"
  "concrete-optimizations.rkt")
(require (only-in racket/base parameter? for in-range symbol?))

; Set up the machine and execute each instruction.
; Properties are proved at the end of the execution of the machine.

(define-syntax-rule (while test body ...) ; while loop
  (let loop () (when test body ... (loop))))

(define-syntax-rule (try! var val body ...) ; try!
  (let ()
    (define var val)
    (if (symbol? var) var
        (begin body ...))))

(define (step m)
  (define pc (machine-pc m))
  (try! next_instr (if (use-sym-optimizations)
                       (fresh-symbolic next_instr (bitvector 32))  ; fetch arbitrary instruction
                       (get-next-instr m)) ; fetch actual instruction
        (define decoded_instr (execute m next_instr))
        ; (printf "PC: ~x BYTES: ~a INS: ~a~n" (bitvector->natural pc) next_instr decoded_instr)
        decoded_instr))
(provide step)

; get instructions until reach mret
(define (execute-until-mret m)
  (let loop ([decoded_instr (step m)])
    (cond
      [(or (equal? decoded_instr '(mret))
           (equal? decoded_instr 'illegal-instruction))
       decoded_instr]
      [else (loop (step m))])))
(provide execute-until-mret)

; get instructions until reach ecall
(define (execute-until-ecall m)
  (let loop ([decoded_instr (step m)])
    (cond
      [(or (equal? decoded_instr '(ecall))
           (equal? decoded_instr 'illegal-instruction))
       decoded_instr]
      [else (loop (step m))])))
(provide execute-until-ecall)

; ; example execution
; (define program (file->bytearray "build/pmp.bin"))
; (printf "~n* Running pmp.bin test ~n")
; (define ramsize 10000)
; (define m (init-machine-with-prog program ramsize))
; (execute-until-mret m)
