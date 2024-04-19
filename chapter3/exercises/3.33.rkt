#lang racket

(require 
  "constraints.rkt"
  rackunit)

(define (averager! a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder! a b u)
    (multiplier! c v u)
    (constant! 2 v)))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(connector-probe! "A" a)
(connector-probe! "B" b)
(connector-probe! "C" c)

(averager! a b c)
(check-false (has-value? a))
(check-false (has-value? b))
(check-false (has-value? c))

(set-value! a 5 'me)
(set-value! b 3 'me)
(check-eq? (get-value a) 5)
(check-eq? (get-value b) 3)
(check-eq? (get-value c) 4)

(forget-value! a 'me)
(set-value! a 8 'me)
(check-eq? (get-value a) 8)
(check-eq? (get-value b) 3)
(check-equal? (get-value c) (/ 11 2))

(forget-value! b 'me)
(set-value! c 0 'me)
(check-eq? (get-value a) 8)
(check-eq? (get-value c) 0)
(check-eq? (get-value b) -8)
