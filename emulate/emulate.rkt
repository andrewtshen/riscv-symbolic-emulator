#lang rosette/safe

(require "load.rkt")
(require "decode.rkt")
(require "execute.rkt")

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
    (execute next_instr m)
    ; we step the pc in execute.rkt
    )


(define x5 (gprs-get-x m 15))
(define x6 (gprs-get-x m 16))
(define x7 (gprs-get-x m 17))
(define model-add (verify (begin 
    (assert (bveq (bvadd x5 x6) x7))
)))

(printf "~a~n" (gprs-get-x m 15))
(printf "~a~n" (gprs-get-x m 16))
(printf "~a~n" (gprs-get-x m 17))
    
; TODO add more instructions
; Add symbolic execution of risc-v instructions, interp.rkt?