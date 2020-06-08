#lang rosette

(define (interpret formula)
  (print formula)
  (display "\n")
  (match formula
    [`(plus, expr ...) (apply + (map interpret expr))]
    [`(minus ,expr ...) (apply - (map interpret expr))]
    [`(equals ,expr ...) (apply eq? (map interpret expr))]
    [lit (constant lit integer?)]
    ))

(solve (assert (interpret `(equals x y))))