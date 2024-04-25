#lang racket

(require
  "../../modules/constraints.rkt"
  rackunit)

(define (celsius-fahrenheit-converter-arithm! x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ a b)
  (let ((s (make-connector)))
    (adder! a b s)
    s))

(define (c* a b)
  (let ((p (make-connector)))
    (multiplier! a b p)
    p))

(define (c/ num denom)
  (let ((quotient (make-connector)))
    (multiplier! quotient denom num)
    quotient))

(define (cv value)
  (let ((var (make-connector)))
    (constant! value var)
    var))

(define C (make-connector))
(define F (celsius-fahrenheit-converter-arithm! C))

(set-value! C 20 'me)
(check-eq? (get-value C) 20)
(check-eq? (get-value F) 68)

(forget-value! C 'me)
(set-value! F -139 'me)
(check-eq? (get-value F) -139)
(check-eq? (get-value C) -95)
