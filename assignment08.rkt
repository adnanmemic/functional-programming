#lang racket

(define (integers-hallo x)
    (s-cons x (integers-hallo (+ x 1))))

(define integers (integers-hallo 1))

(define-syntax-rule (s-delay exp)
  (λ() exp))

(define-syntax-rule (s-cons a b)   
  (cons a (s-delay b)))

(define (s-first s)
  (car s))

(define (s-rest s) 
  (s-force (cdr s)))

(define (s-force delayedObject)
  (delayedObject))

(define (s-empty? s)
  (eq? s empty-s))

(define empty-s 'S-EMPTY-STREAM)

(define myStream (s-cons 1 (s-cons 2 (s-cons 3 empty-s))))

;--------------Exercise 1---------------

(define (mySum s)
  (define (helper s acc)
    (s-cons acc (helper (s-rest s) (+ acc (s-first s)))))
  (helper (s-rest s) (s-first s)))

;Test 
(define (s-display-limit s limit)
  (if (or (= limit 0) (s-empty? s))
      "...?"
      (~a (s-first s) "," (s-display-limit (s-rest s) (- limit 1)))))

(s-display-limit (mySum integers) 8)

;--------------Exercise 2---------------

;Beta reduction
;(λx.λy.λz. - (+ x y) z) 1 2 3
;(λy.λz. - (+ 1 y) z) 2 3
;(λz. - (+ 1 2) z) 3
; - (+ 1 2) 3)
; - 3 3
;0

;--------------Exercise 3---------------

;a)
;(λx.λy.(λx. + x x) y) 5 m
;(λx.λy.(λz. + z z) y) 5 m    

;b)
;(λx.λy.(λz. + z z) y) 5 m
;(λy.(λz. + z z) y) m
;(λz. + z z) m
; + m m

;--------------Exercise 4---------------

;(λx. λy. (- x ((λf. (* f 2)) y))) 7 3
;(λy. (- 7 ((λf. (* f 2)) y))) 3
; - 7 ((λf. (* f 2)) 3)
; - 7 (* 3 2)
; - 7  6
; 1

(define (g x y)
 (define (f x) (* x 2))
 (- x (f y)))

(g 7 3)
