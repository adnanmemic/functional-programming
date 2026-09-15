#lang racket
(require racket/trace)

;------------------------Exercise 1------------------------------

(define (cuberoot x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess guess) x)) 0.001))

  (define (improve guess)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

  (define (cuberoot-iter guess)
    (if (good-enough? guess)
        guess
        (cuberoot-iter (improve guess)))) ;

  (cuberoot-iter 1.0))

;Test
(cuberoot 27)

;------------------------Exercise 2------------------------------

(require racket/trace)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.001))

  (define (improve guess)
    (/ (+ (/ x guess) guess) 2))

  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))

  
  (trace sqrt-iter)
  (sqrt-iter 1.0))

;Test

;------------------------Exercise 3------------------------------

(define (powerCloseTo b n)

  (define (bigger? e)
    (if (> (expt b e) n)
        e
        (bigger? (+ e 1)))) 

  (bigger? 1))

;Test
(powerCloseTo 5 20)

;------------------------Exercise 4------------------------------

(define (myif predicate then-clause else-clause)
  
 (cond (predicate then-clause)
 (else else-clause)))

;my-if Test
(myif (= 2 3) 0 5)
(myif (= 1 1) 0 5)


(define (sqrt-myif x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.001))

  (define (improve guess)
    (/ (+ (/ x guess) guess) 2))

  (define (sqrt-iter guess) 
    (if (good-enough? guess) 
        guess
        (sqrt-iter (improve guess))))

  
  (trace sqrt-iter) 
  (sqrt-iter 1.0))

;Test-myif-Newton
(sqrt-myif 16)

;------------------------Exercise 5------------------------------

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(trace fib)
;Test
(fib 5)

(define (fib-iter n)
  (define (help a b count)
    (if (= count (- n 1))
        b
        (help b (+ a b) (+ count 1))))
  
  (trace help)
  (help 0 1 0))

;Test
(fib-iter 5)
