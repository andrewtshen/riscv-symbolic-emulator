#lang slideshow

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(four (circle 20))
(four (rectangle 20 20))

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple"  "violet" "brown" "black")))

(rainbow (circle 10))