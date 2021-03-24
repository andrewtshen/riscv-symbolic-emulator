#lang rosette/safe

(require
  "init.rkt"
  "execute.rkt"
  "execute-compressed.rkt"
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

; Get next instruction using current program counter
(define (get-next-instr m size)
  (define pc (machine-pc m))
  (machine-ram-read m pc (/ size 8)))
(provide get-next-instr)

(define (step m)
  (define pc (machine-pc m))
  
  (define instr16 (if (use-sym-optimizations)
                       (fresh-symbolic next_instr (bitvector 16))  ; fetch arbitrary instruction
                       (get-next-instr m 16)))                     ; fetch actual instruction
  (cond
    [(equal? instr16 'illegal-instruction)
     (illegal-instr m)
     instr16]
    ; [(bveq (bvand instr16 (bv #b11 16)) (bv #b11 16))
    [(bveq (extract 1 0 instr16) (bv #b11 2))
     ; Regular word instruction
     (define next_instr (if (use-sym-optimizations)
                            (fresh-symbolic next_instr (bitvector 32))  ; fetch arbitrary instruction
                            (get-next-instr m 32)))                     ; fetch actual instruction
     ; TODO: Implement actual exception handler
     (cond
       [(symbol? next_instr)
        (illegal-instr m)
        next_instr]
       [else
        (define decoded_instr (execute m next_instr))
        (when (use-debug-mode)
          (printf "PC: ~x BYTES: ~a INS: ~a~n" (bitvector->natural pc) next_instr decoded_instr))
        (when (symbol? decoded_instr)
          (illegal-instr m))
        decoded_instr])]
    [(bveq instr16 (bv 0 16))
     'illegal-instruction]
    [else
     ; Compressed instruction (halfword)
     (define decoded_instr (execute-compressed m instr16))
     (when (use-debug-mode)
          (printf "PC: ~x BYTES: ~a INS: ~a~n" (bitvector->natural pc) instr16 decoded_instr))
     (when (symbol? decoded_instr)
       (illegal-instr m))
     decoded_instr]))
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
