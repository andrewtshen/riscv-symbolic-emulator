#lang rosette/safe

(require
  "fmt.rkt"
  "machine.rkt"
  "parameters.rkt"
  "instr.rkt")

; Decode and execute all of the binary instructions and instruction as list

; Execute a 32 bit instruction
(define (execute-compressed m b_instr)
  ; TODO: Implement execute-compressed
  'illegal-instruction)
  ; (define opcode (extract 6 0 b_instr))
  ; (define fmt (get-compressed-fmt opcode))
  ; (cond
  ;   [(eq? fmt 'R) (execute-R m b_instr)]
  ;    (execute-FENCE m b_instr)]
  ;   [else
  ;    ; (printf "No such FMT ~n")
  ;    'illegal-instruction]))
(provide execute-compressed)

; example: add x5, x6, x7
; (define b_instr (bv #b00000000011100110000001010110011 32))
; (define b_instr (bv #x11111111 32))
; (define instr (execute b_instr))
; (printf "~a~n" instr)
