#lang racket

(require rackunit)

(provide variable?
         same-variable?
         =number?
         sum?
         addend
         augend
         product?
         multiplier
         multiplicand
         make-exponentiation
         exponentiation?
         base
         exponent
         make-sum
         make-product)


;; From the book
(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (number? num) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((=number? m1 m2) (make-exponentiation m1 2))
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? e)
  (and (list? e) (eq? (car e) '+)))

(define (addend e)
  (cadr e))

(define (augend e)
  (caddr e))

(define (product? e)
  (and (list? e) (eq? (car e) '*)))

(define (multiplier e)
  (cadr e))

(define (multiplicand e)
  (caddr e))


(define (derive expr variable)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr variable) 1 0))
        ((sum? expr)
         (make-sum (derive (addend expr) variable)
                   (derive (augend expr) variable)))
        ((product? expr)
         (let ((m1 (multiplier expr))
               (m2 (multiplicand expr)))
           (make-sum (make-product m1 (derive m2 variable))
                     (make-product (derive m1 variable) m2))))
        ;; Solution
        ((and (exponentiation? expr) (number? (exponent expr)))
         (let ((expr-base (base expr))
               (expr-exponent (exponent expr)))
           (make-product (make-product expr-exponent
                                       (make-exponentiation expr-base (- expr-exponent 1)))
                         (derive expr-base variable))))
        (else (error "Cannot derive " expr))))

(define (exponentiation? expression)
  (and (list? expression) (eq? '** (car expression))))

(define (base exponentiation)
  (cadr exponentiation))

(define (exponent exponentiation)
  (caddr exponentiation))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(check-equal? (derive '(** x 3) 'x) '(* 3 (** x 2)))
(check-equal? (derive '(** x 2) 'x) '(* 2 x))
(check-equal? (derive '(** x 1) 'x) 1)
(check-equal? (derive '(** x 0) 'x) 0)
(check-equal? (derive '(** y 2) 'x) 0)
