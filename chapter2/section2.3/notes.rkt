#lang racket

(require rackunit)


(define (memq item sequence)
  (cond ((null? sequence) false)
        ((eq? item (car sequence)) sequence)
        (else (memq item (cdr sequence)))))

(check-false (memq 'apple '(banana pear orange)))
(check-false (memq 'apple '(bananas pears apples)))
(check-equal? (memq 'apple '(banana pear apple)) '(apple))
(check-equal?
 (memq 'apple '(banana (apple peach) apple pear)) '(apple pear))

(define (variable? e)
  (symbol? e))
;; (variable? 'x) => #t
;; (variable? 'ab) => #t
;; (variable? '(+ ab x)) => #f

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
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


;; The trick of developing with abstractions first without an explicit
;; implementation is pretty neat. It forces you to design good
;; abstraction barriers, eliminating the temptation of reaching into
;; the data structures, as yet unknown.

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

;; Examples
(check-equal? (derive '(+ x 3) 'x) 1)
(check-equal? (derive '(* x y) 'x) 'y)
(check-equal?  (derive '(* (* x y) (+ x 3)) 'x)
               '(+ (* x y) (* y (+ x 3))))
