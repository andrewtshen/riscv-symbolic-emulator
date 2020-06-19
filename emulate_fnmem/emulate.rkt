#lang rosette/safe

(require
  "init.rkt"
  "decode.rkt"
  "execute.rkt"
  "machine.rkt"
  "pmp.rkt"
  "parameters.rkt"
  "print_utils.rkt")
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

  (when (use-debug-mode)
    (printf "next_instr: ~v~n" next_instr))

  (cond
    [(not (eq? next_instr null))
      (define decoded_instr (decode m next_instr))
      (when (use-debug-mode)
        (printf "decoded_instr: ~a~n" decoded_instr))
      (cond
        [(not (eq? decoded_instr null))
          (when (use-debug-mode)
            (printf "execute instr: ~a~n" decoded_instr))
          (execute decoded_instr m)]
        [else null])]
    [else null]))
(provide step)

; get instructions until reach mret
(define (execute-until-mret m)
  (define op null)
  (while (not (eq? op 'mret))
    (define next_decoded_instr (step m))
    (when (use-debug-mode)
      (printf "PC: ~x INS: ~a~n" (bitvector->natural (get-pc m)) next_decoded_instr))
    (cond
      [(eq? next_decoded_instr null) null]
      [else
        (set! op (list-ref next_decoded_instr 0))])))
(provide execute-until-mret)

; ; example execution
; (define program (file->bytearray "build/pmp.bin"))
; (printf "~n* Running pmp.bin test ~n")
; (define ramsize 10000)
; (define m (init-machine-with-prog program ramsize))
; (execute-until-mret m)
