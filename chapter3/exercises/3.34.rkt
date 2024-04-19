#lang racket

(require
  "constraints.rkt"
  rackunit)

;; Louis Reasoner wants to build a squarer, a
;; constraint device with two terminals such that the value
;; of connector b on the second terminal will always be the
;; square of the value a on the ﬁrst terminal. He proposes the
;; following simple device made from a multiplier:

(define (squarer! a b)
  (multiplier! a a b))

;; There is a serious ﬂaw in this idea. Explain.

;; Before trying the code, my guess is that the problem occurs when
;; calculating the inverse. If we set the value of connector b, the
;; multiplier box will refuse to calculate a, since it think there
;; are two different unknowns.

;; Testing my hypothesis
(define a (make-connector))
(define b (make-connector))
(squarer! a b)

(set-value! a 2 'me)
(check-eq? (get-value b) 4)

(forget-value! a 'me)
(set-value! b 4 'me)
(check-false (has-value? a))  ; This test succeeds, confirms hypothesis
