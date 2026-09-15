#lang racket

;Interpreter 1
(define (mc-eval exp [env null])
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))
        ((pair? exp) (mc-apply (mc-eval (car exp) env) (list-of-values (cdr exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (mc-apply procedure arguments)
  (cond ((tagged-list? procedure 'primitive) (apply-primitive-procedure procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))

(define (lookup-variable-value var env)
      (define val (assq var env))
      (if (eq? val false)
          (error "unbound variable" var)
          (cdr val)))

(define (list-of-values exps env)
  (if (null? exps)
      '()
      (cons (mc-eval (car exps) env) (list-of-values (cdr exps) env))))
  
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-racket (car (cdr proc)) args))

(define apply-in-underlying-racket apply)

;--------------Exercise 1---------------

(define env `((x . 10) (y . 20)
              (+ . ,(list 'primitive +))
              (- . ,(list 'primitive -))
              (* . ,(list 'primitive *))
              (/ . ,(list 'primitive /))))

;Test
(displayln (mc-eval 'x env)) 
(displayln (mc-eval '(- x y) env)) 

(define test-expr1 '(+ 1 2))
(define test-expr2 '(* 3 4))

(displayln (mc-eval test-expr1 env)) 
(displayln (mc-eval test-expr2 env))

;--------------Exercise 2---------------

(define (findFirst pred lst #:noMatch [noMatch #f])
  (define (helper lst)
    (cond
      [(empty? lst) noMatch]
      [(pred (first lst)) (first lst)]
      [else (helper (rest lst))]))
  (helper lst))

;Test
(findFirst even? '(1 3 5 6 7 8))
(findFirst even? '(1 3 5 7 9))
(findFirst even? '(1 3 5 7 9) #:noMatch 'keinTreffer)

;--------------Exercise 3---------------

;Interpreter 1
(define (lookup-variable-value1 var env)
  (define help (findFirst (λ(x) (eq? (car x) var)) env))
      (if (eq? help #f)
          (error "unbound variable" var)
          (cdr help)))

;Test
(define e (list (cons 'a 5) (cons 'b 6)))

(lookup-variable-value1 'a e)
(lookup-variable-value1 'b e)


;Interpreter 2
(define (lookup-variable-value2 var env)
  (define (findInFrame frame)
    (findFirst (lambda (x) (eq? (car x) var)) frame))
  (define (helper env)
    (cond ((null? env) (error "unbound variable" var))
          (else
           (define help (findInFrame (car env)))
           (if (eq? help #f)
               (helper (cdr env))
               (cdr help)))))
  (helper env))

;Test
(define e1 (list (list (cons 'a 1) (cons 'b 2))
                 (list (cons 'c 3) (cons 'd 4))
                 (list (cons 'e 5))))

(lookup-variable-value2 'c e1)

;--------------Exercise 5---------------

(define (g)
  (g))

(mc-eval (g) null)
