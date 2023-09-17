#lang racket

(require
  "../../utils.rkt"
  rackunit)


(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (node)
          (cond ((null? node) 0)
                ((list? node) (count-leaves node))
                (else 1)))
        t)))

(check-equal? (count-leaves '(1 (2 3) (5 (6)))) 5)
