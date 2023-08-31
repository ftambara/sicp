#lang racket

(require rackunit
         "basebot.rkt")

; Problem 1
(check-equal? (position 0 0 0 0) 0)
(check-equal? (position 0 0 20 0) 20)
(check-equal? (position 0 5 10 10) 60)
(check-equal? (position 2 2 2 2) 10.0)
(check-equal? (position 5 5 5 5) 92.5)

; Problem 2
(check-equal? (root1 1 2 0) -2.0)
(check-equal? (root2 1 2 0) 0)
(check-equal? (root1 -.2 .8 1) 5.0)
(check-within (root2 -.2 .8 1) -1.0 0.01)
(check-false (root1 5 3 6))
(check-false (root2 5 3 6))
