#lang racket

;Number 1----------------------------
(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b )

(if (and (> b a) (< b (* a b)))
    b
    a)

;(-4)

;(- 4)

-4

- 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
      ((< a b) b)
      (else -1))
   (+ a 1))

((if (< a b) + -) a b)


;Number 2------------------------------
(define (sign x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        ((= x 0) 0)))

(sign 5)

(sign -5)

(sign 0)


;Number 3------------------------------
(define (square z) (* z z))

(define (sumSquareBigger m n s)
  (cond ((and (> m s) (> n s)) (+ (square m) (square n)))
        ((and (> n m) (> s m)) (+ (square n) (square s)))
        ((and (> s n) (> m n)) (+ (square s) (square m)))))


(sumSquareBigger 1 2 3)
(sumSquareBigger 4 2 3)
(sumSquareBigger 4 5 3)


;Number 4------------------------------
;a)
(define (areaRect length width) (* length width))

(areaRect 3 4) 

;b)
(define (circumferenceCircle r) (* 2 r pi))

(circumferenceCircle 5)


;Number 5------------------------------
;a)
(define (mynot e f)
  (if(> e f)
     #f
     #t))

(mynot 3 4)

;b)

(if (and (> 1 5) (> 6 2))
    #t
    #f) 

(if (or (< 1 5) (< 6 2))
    #t
    #f) 
