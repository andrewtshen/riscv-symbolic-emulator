#lang racket
(provide (all-defined-out))
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
     (printf "hey\n")]
    [(equal? "bye" (substring s 0 3))
     (printf "see ya later\n")]
    [else (printf "what did you say?\n")]))

(define (double v)
  ((if (string? v) string-append *) v v))

(define (triple f x)
  (f (f (f x))))

(let* ([x (random 10)]
         [o (random 10)]
         [diff (number->string (abs (- x o)))])
    (cond
     [(> x o) (printf (string-append "X wins by " diff "\n"))]
     [(> o x) (printf (string-append "O wins by " diff "\n"))]
     [else "cat's game\n"]))


; List Usage
(printf "List Usage\n")
(printf (string-append "list length: " (number->string (length (list "hop" "skip" "jump"))) "\n"))
(printf (string-append "list 2nd index: " (list-ref (list "hop" "skip" "jump") 2) "\n"))

; List mappings
(map (lambda (i)
       (+ i 1))
     (list 1 2 3 4))

(filter number? (list 1 2 "hello" 3))

(first (list 1 2 3 "hello" "world"))
(rest (list 1 2 3 "hello" "world"))

; Keyword Arguments
(define (sub x y) (- x y))
(sub 12 30) ; -18
(define (kw-sub #:foo foo-val y) (sub foo-val y))
(kw-sub 12 #:foo 18) ; 18

; Set Default Values
(define (add-defaults x [y 10] #:z [z 1]) (+ x y z))
(add-defaults 100 20 #:z 2) ; 121
(add-defaults 500) ; 511

(define (remove-dups l)
  (display l)
  (printf " ")
  (display (rest l))
  (printf "\n")
  (cond
   [(empty? l) empty] ; check if the initial list empty
   [(empty? (rest l)) l] ; check if the rest of the list is empty
   [else
    (let ([i (first l)])
      (if (equal? i (first (rest l))) ; check if the first element is equal to the second element
          (remove-dups (rest l)) ; if true, discard the first element
          (cons i (remove-dups (rest l)))))])) ; if false, keep the first element
(remove-dups (list 1 1 2 1 2 2))


