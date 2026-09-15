#lang racket

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

;------------------Exercise 1--------------------------

(define (checkProp predicate y)
  (if (null? y)
      '()
      (cons (predicate (car y))
            (checkProp predicate (cdr y)))))

;Test
(checkProp even? (list 1 2 3 4))

(define (checkPropOneLine predicate y)
  (map predicate y))

;Test
(checkPropOneLine even? (list 1 2 3 4))

;-------------------Exercise 2--------------------------

(define (doAll proc-list xs)
  (map (lambda (proc) (proc xs)) proc-list))

;Test
(doAll (list sqrt square cube) 4)
(doAll (list length car cdr) (list 1 2 3))

;-------------------Exercise 3---------------------------

;a)
(define (toCelsius x)
  (map (lambda (f) (* (- f 32) (/ 5 9))) x))

;Test
(toCelsius (list -40 32 50))


;b)
(define (checkAll predicate xs)
  (foldr(lambda (item x) (and (predicate item) x))#t xs))

;Test
(checkAll even? (list 2 4 6 7 8))

;--------------------Exercise 4----------------------------

;a)
(define (countEvenGreater20 xs)
  (count (lambda (x) (and (even? x) (> x 20))) xs))

;Test
(countEvenGreater20 (list 1 4 22 24 26 33 -44))

;b)

(define (sumEvenGreater20 xs)
  (apply + (filter (lambda (x) (and (even? x) (> x 20))) xs)))

;Test
(sumEvenGreater20 (list 1 4 22 24 26 33 -44))

;-------------------Exercise 5----------------------------

(define (even-sum)
  (foldr + 0 (filter (lambda (x) (= (remainder x 2) 0))
                     (range 10 101))))

;Test
(even-sum)
