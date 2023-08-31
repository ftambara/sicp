#lang racket

(require rackunit
         "basebot.rkt")

(check-equal? (position 0 0 0 0) 0)
(check-equal? (position 0 0 20 0) 20)
(check-equal? (position 0 5 10 10) 60)
(check-equal? (position 2 2 2 2) 10.0)
(check-equal? (position 5 5 5 5) 92.5)
