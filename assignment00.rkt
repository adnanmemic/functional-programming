#lang racket

(+ (- (* 2 (+ 11 9)) (* 4 5)) 23)

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(fact 5)

(define (isEven m)
  (= (remainder m 2) 0))

(display (isEven 3))
(display (isEven 4))
