 #lang racket

(require racket/stream)

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

(define myStream (s-cons 100 (s-cons 123 (s-cons 142 empty-s))))

;-------------Exercise 1--------------

;a)
(define (s-length xs)
  (if(s-empty? xs)
     0
     (+ 1 (s-length (s-rest xs)))))

;Test
(s-length myStream)

;b)
(define (s-map proc s)
  (if (s-empty? s)
      empty-s
      (s-cons (proc (s-first s)) (s-map proc (s-rest s)))))

  (define (toCelsius-stream xs)
    
    (define (help xs)
      (s-map (λ(x)(- x 32)) xs))
    
    (s-map (λ(x)(exact->inexact(* x (/ 5 9)))) (help xs)))

;Test
(define (s-display s)
  (if (s-empty? s)
      ""
      (~a (s-first s) "," (s-display (s-rest s)))))

(s-display (toCelsius-stream myStream))

;-------------Exercise 2--------------

(define (list2s xs)
  (if (empty? xs)
      empty-s
      (s-cons (car xs) (list2s (cdr xs)))))

;Test 
(s-display (list2s (list 1 2 3 4)))

(define (s2list xs)
  (if (s-empty? xs)
      '()
      (cons (s-first xs) (s2list (s-rest xs)))))

;Test
(define d (s-cons 1 (s-cons 2 (s-cons 3 empty-s))))

(s2list d)

;-------------Exercise 3--------------

(define (powersOf2 x)
      (s-cons x (powersOf2 (* x 2))))

;Test
(define (s-display-limit s limit)
  (if (or (= limit 0) (s-empty? s))
      "...?"
      (~a (s-first s) "," (s-display-limit (s-rest s) (- limit 1)))))

(s-display-limit (powersOf2 64) 10)
