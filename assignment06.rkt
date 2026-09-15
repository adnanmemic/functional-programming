#lang racket

;----------Exercise 3---------

(define (mymap p xs)
 (foldr (lambda (x y) (cons (p x) y)) null xs))

;Test
(mymap even? (list 1 2 3 4))

(define (myappend xs ys)
  (foldr cons ys xs))

;Test
(myappend (list 1 2 3) (list 4 5 6))

(define (mylength xs)
 (foldr (lambda (el count) (+ 1 count)) 0 xs))

;Test
(mylength (list 1 2 3 5))

;----------Exercise 4---------

(require math/statistics)

(define (rain xs)

  (define (valid-rainfall xs)
    (if (= (first xs) -999)
        '()
        (cons (first xs) (valid-rainfall (rest xs)))))
        
  
  (define positive-rainfall
    (for/list ([x (valid-rainfall xs)]
               #:when (> x 0))
      x))

  (mean positive-rainfall))

;Test
(rain (list 1 2 -100 -999 1000))

;----------Exercise 5----------

(define (length xs)
  (match xs
    ['() 0]
    [(cons head tail) (+ 1 (length tail))]))

;Test
(length (list 1 2 3 4 10))

(define (filter y xs)
  (match xs
    ['() 0]
    [(cons head tail) (if (y head)
                          (+ 1 (filter y tail))
                          (filter y tail))]))

;Test
(filter even? (list 5 5 5 4 4 7 8 5 5))
