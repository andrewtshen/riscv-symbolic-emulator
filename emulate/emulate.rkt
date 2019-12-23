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
    ; (define next_instr (list "mret"))
    (printf "~a~n" next_instr)
    (set! op (list-ref next_instr 0))
    (printf "~a~n" op)
    (set-pc! m (+ (get-pc m) 1)))
    

; (define operation "mret")
; (define mylist (list operation))

; (set! op (list-ref mylist 0))
; (printf "~a~n" op)
; (printf "~a~n" (equal? op "mret"))