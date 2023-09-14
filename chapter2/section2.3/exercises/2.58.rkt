#lang racket

(require rackunit)


;; From the book

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (number? num) (= exp num)))

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
        (else (error "Cannot derive " expr))))

;; Solutions

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? expr)
  (and (list? expr) (eq? (cadr expr) '+)))

(define (addend sum)
  (car sum))

(define (augend sum)
  (caddr sum))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? expr)
  (and (list? expr) (eq? (cadr expr) '*)))

(define (multiplier prod)
  (car prod))

(define (multiplicand prod)
  (caddr prod))


(check-equal? (derive '(x + 3) 'x) 1)
(check-equal? (derive '(x * y) 'x) 'y)
(check-equal?  (derive '((x * y) * (x + 3)) 'x)
               '((x * y) + (y * (x + 3))))
