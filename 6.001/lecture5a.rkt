#lang racket

;; Terminology
;;
;; (lambda (x) (proc x))
;;  ^       ^
;;  |       bound variable
;;  quantifier
;;
;; bound variable: functionality doesn't change if I rename the variable
;;
;; (lambda (x) (proc x y))
;;                     ^
;;                     free variable
;;
;; (lambda (x) (proc x))
;;             <      >
;;            scope of x

;; Once we introduce the ability to change the values of free variables, the
;; substitution model is no longer adequate for modeling procedure application.
;; We need a new computational model: the environment model.
;;
;; Environments are a sequence of superimposed frames which give values to
;; bound variables. New frames correspond with the application of procedures.
;; A procedure consists of both code and an environment.
;;
;; Application rules:
;; 1. Construct a frame which binds the formal parameters to the arguments
;;      of the call. Evaluate the body of the procedure in the context of
;;      the new environment built.
;;      The enclosing environment of the new frame will be the one active
;;      before applying the procedure (the environment part of the procedure
;;      being applied)
;; 2. Evaluation of lambda expressions. Build a procedure object. It's code
;;      is combined with a pointer to its environment.

(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (+ n 1))
      n)))

(require rackunit)

;; Monte carlo simulations are an example of a good reason to use state
;; (Here we don't use set because racket does it for us with 'random'
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro))))

(define (cesaro)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter passed remaining)
    (cond ((= remaining 0)
           (/ passed trials))
          ((experiment)
           (iter (+ passed 1) (- remaining 1)))
          (else
            (iter passed (- remaining 1)))))
  (iter 0 trials))

(define rand
  (lambda () (random 65536)))

(check-within (estimate-pi 1000000)
              pi
              ;; 1% error
              (* pi 0.01))

;; If we aren't willing to accept a non-functional procedure like 'cesaro',
;; Our only choice is to have a monolithic function like 'random-gcd-test'.
;; We loose our ability to make abstractions.
(define (estimate-pi-functional trials)
    (sqrt (/ 6 (random-gcd-test trials))))

(define (random-gcd-test trials)
  (define (iter passed remaining)
    (let ((x1 (random 65536))
          (x2 (random 65536)))
      (cond ((= remaining 0)
             (/ passed trials))
            ((= (gcd x1 x2) 1)
             (iter (+ passed 1) (- remaining 1)))
            (else
              (iter passed (- remaining 1))))))
  (iter 0 trials))

(check-within (estimate-pi-functional 1000000)
              pi
              ;; 1% error
              (* pi 0.01))
