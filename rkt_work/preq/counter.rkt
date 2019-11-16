#lang rosette

(struct counter (pos neg) #:mutable)

(define (counter_val idx)
  (vector-ref (counter_val) idx))

(define (set_counter idx)
  (vector-ref (set_counter) idx))

(define (execute opcode)
  (case opcode
    [(inc)
     (set_counter 0 (+ 1 (counter_val 0)))]
    [(dec)
       (set_counter 1 (+ 1 (counter_val 1)))]))

(define-symbolic X Y integer?)
(define c (counter X Y))
(define program
 (`(inc)))