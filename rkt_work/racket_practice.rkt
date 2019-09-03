#lang racket

(printf (string-append "rope" "twine" "yarn" "\n"))
(define (sqrt-wrapper i)
  (if (equal? 8 (sqrt i))
      (printf "Square root is 8\n")
      (printf "Square root isn't 8\n")))

(sqrt-wrapper 64)

(if (equal? "hello world" (string-append "hello " "world"))
    (printf "They are equal!\n")
    (printf "They aren't equal!\n")
    )

(define (reply s)
  (cond
    [(equal? "hello" (substring s 0 5))
     (printf "hey")]
    [(equal? "bye" (substring s 0 3))
     (printf "see ya later")]
    [else "what did you say?"]))

(define (double v)
  ((if (string? v) string-append *) v v))

(define (triple f x)
  (f (f (f x))))

(let* ([x (random 10)]
         [o (random 10)]
         [diff (number->string (abs (- x o)))])
    (cond
     [(> x o) (printf (string-append "X wins by " diff))]
     [(> o x) (printf (string-append "O wins by " diff))]
     [else "cat's game\n"]))