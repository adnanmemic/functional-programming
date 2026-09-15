#lang racket

;--------------Exercise 1-------------

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (min-fx-gx f g x)
  (min (f x) (g x)))

;Test
(min-fx-gx square cube -1)
(min-fx-gx square cube 2)

(define (combine-fx-gx result f g x)
  (result (f x) (g x)))

;Test
(combine-fx-gx min square cube -1)
(combine-fx-gx max square cube -1)

;--------------Exercise 2-------------

(define (f g)
 (g 5))

;Test
(f +)
(f square)
(f (lambda (x) (* x (+ x 2))))

;--------------Exercise 3-------------

;a)
(define (fr n)
  (if (< n 3)
      n
      (+ (fr (- n 1))
         (* 2 (fr (- n 2)))
         (* 3 (fr (- n 3))))))

;Test
(fr 2)
(fr 4)

;b)
#|
(define (fi n)
  (define (help a b c count)
    (if (< n 3)
      n
      (help 2 1 0 n))
    (if (= count 2)

         (iter b
            c
            (+ (* 3 c) (* 2 b) a) (- count 1)
  |#    

;--------------Exercise 4-------------

;a)
(define (twice y)
  (lambda (x) (y (y x)))) 

(define square-twice (twice square))

;Test
(square-twice 4) 

;b)
(define (comp a b)
  (lambda (x) (a (b x))))

(define (inc x) (+ x 1))

;Test
((comp cube inc) 2)
((comp inc cube) 2)

;--------------Exercise 5-------------

(define (mycons x y)
  (define (dispatch m)
    (cond [(= m 0) x]
          [(= m 1) y]
          [else (error "argument not 0 or 1 -- in mycons" m)]))
dispatch)

(define (mycar z) (z 0))
(define (mycdr z) (z 1))

(define (make-complex real imaginary)
  (mycons real imaginary))

(define (real-part complex)
  (mycar complex))

(define (imaginary-part complex)
  (mycdr complex))

(define (add-complex c1 c2)
  (make-complex (+ (real-part c1) (real-part c2))
                (+ (imaginary-part c1) (imaginary-part c2))))

(define (print-complex complex)
  (display "(")
  (display (real-part complex))
  (display " + ")
  (display (imaginary-part complex))
  (display "i)")
  (newline))

;Test
(define c1 (make-complex 6 5))
(define c2 (make-complex 4 5))

(print-complex (add-complex c1 c2))
