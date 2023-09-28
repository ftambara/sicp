#lang racket

(require "../../section2-3/exercises/2.56.rkt")


; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp) (if (same-variable? exp var) 1 0))
;         ((sum? exp)
;          (make-sum (deriv (addend exp) var)
;                    (deriv (augend exp) var)))
;         ((product? exp)
;          (make-sum (make-product
;                     (multiplier exp)
;                     (deriv (multiplicand exp) var))
;                    (make-product
;                     (deriv (multiplier exp) var)
;                     (multiplicand exp))))
;         ; ⟨more rules can be added here⟩
;         (else (error "unknown expression type: DERIV" exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (get op type) (display "Not implemented yet"))
(define (put op type fn) (display "Not implemented yet"))

;; a.
;; Instead of checking for the type of the expression through
;; explicit predicates, they dispatch all compound expressions
;; with its operand.
;; To remove the explicit number? and variable? checks we would need
;; to dispatch on the whole expression, since we wouldn't know if we
;; have an operator to extract. This raises the question of what part
;; of the expression should determine the type of deriv to call.


;; b.
(define (install-sum-package)
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'deriv '+ deriv-sum)

  'done)

(define (install-prod-package)
  (define (deriv-prod exp var)
    (make-sum
     (make-product (multiplier exp) (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var) (multiplicand exp))))

  (put 'deriv '* deriv-prod)

  'done)


;; c.

(define (install-expt-package)
  (define (deriv-expt exp var)
    (let ((expr-base (base exp))
          (expr-exponent (exponent exp)))
      (make-product (make-product expr-exponent
                                  (make-exponentiation expr-base (- expr-exponent 1)))
                    (deriv expr-base var))))

  (put 'deriv '** deriv-expt)

  'done)


;; d.
;; It should be enough to rename the parameters of get, without even
;; changing its body.
;; If we cannot change that, and the order of put has been flipped
;; too, the calls to put should be flipped too. For example:
;;  (put '** 'deriv deriv-expt)
