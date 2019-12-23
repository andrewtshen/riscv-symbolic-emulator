#lang rosette/safe

(require "load.rkt")
(require "decode.rkt")

(define-syntax-rule (while test body ...) ; while loop
    (let loop () (when test body ... (loop))))

; get program
(define program (file->bitvectors "sum.bin"))

; make machine
(define ramsize 10)
(define m (init-machine program ramsize))

(define op null)
; ; get instructions until reach mret
(while (not (equal? op "mret"))
    (define next_instr (decode (ram-get-x m (get-pc m))))
    (printf "PC: ~a INS: ~a~n" (get-pc m) next_instr)
    (set! op (list-ref next_instr 0))
    (set-pc! m (+ (get-pc m) 1)))
    
; TODO add more instructions
; Add symbolic execution of risc-v instructions, interp.rkt?