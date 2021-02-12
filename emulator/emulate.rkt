#lang rosette/safe

(require
  "init.rkt"
  "decode.rkt"
  "execute.rkt"
  "machine.rkt"
  "pmp.rkt"
  "parameters.rkt"
  "print_utils.rkt"
  "concrete-optimizations.rkt")
(require (only-in racket/base parameter? for in-range))

; Set up the machine and execute each instruction.
; Properties are proved at the end of the execution of the machine.

(define-syntax-rule (while test body ...) ; while loop
  (let loop () (when test body ... (loop))))

(define (step m)
  (define next_instr
    (if (use-sym-optimizations)
        (fresh-symbolic next_instr (bitvector 32)) ; fetch arbitrary instruction
        (get-next-instr m))) ; fetch actual instruction
  ; (define next_instr (bv #x80f10023 32)) ; use a concrete instruction
  ; (printf "next_instr: ~a~n" next_instr)
  (define decoded_instr
    (if (use-concrete-optimizations)
      (concrete-decode next_instr)
      (decode next_instr)))
  ; (printf "decoded_instr: ~a~n" decoded_instr)
  (execute m decoded_instr))
(provide step)

; get instructions until reach mret
(define (execute-until-mret m)
  (let loop ([decoded_instr (step m)])
    ; (printf "PC: ~x INS: ~a~n" (bitvector->natural (machine-pc m)) decoded_instr)
    (unless (or (equal? decoded_instr '(mret)) (equal? decoded_instr null))
      (loop (step m)))))
(provide execute-until-mret)

; ; example execution
; (define program (file->bytearray "build/pmp.bin"))
; (printf "~n* Running pmp.bin test ~n")
; (define ramsize 10000)
; (define m (init-machine-with-prog program ramsize))
; (execute-until-mret m)
