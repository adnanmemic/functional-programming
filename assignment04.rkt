#lang racket

;-------------------Exercise 1----------------------

;a)
(define (mylast xs)
  (if (null? (cdr xs))
      (car xs)
      (mylast (cdr xs))))

;Test
(mylast (list 1 2 3 4 5 6))

;b)
(define (taillength xs)
  
  (define (helper y count)
    (if (null? y)
        count
        (helper (cdr y) (+ count 1))))
  (helper xs 0))

;Test
(taillength (list 1 2 3))

;-------------------Exercise 2----------------------

(define (getFromIdx idx xs)
  (define (helper y count)
    
    (cond [(null? y) '()]
          [(= idx count) y]
          [else (helper (cdr y) (+ count 1))]))
  
  (helper xs 0))

;Test
(getFromIdx 3 (list 0 1 2 3 4))

;-------------------Exercise 3----------------------

(define (combine op xs ys)
  (if (null? xs)          
      '()
      (cons
        (op (car xs) (car ys))
        (combine op (cdr xs) (cdr ys)))))   
 
;Test
(combine + (list 1 2 3) (list 4 5 6))

;-------------------Exercise 4----------------------

(define (isSame xs ys)
  (cond [(and (null? xs) (null? ys)) #t]
        [(or (null? xs) (null? ys)) #f]

        [(= (car xs) (car ys))
         (isSame (cdr xs) (cdr ys))]

        [else #f]))

;Test
(isSame (list 1 2 3) (list 1 6 3))
(isSame (list 1 2 3) (list 1 2 3))
