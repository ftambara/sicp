#lang racket

(require
  "../../utils.rkt"
  rackunit)

(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (elem accu) (+ elem (* accu x)))
   0
   coefficient-sequence))

(check-equal? (horner-eval 2 (list 1 3 0 5 0 1)) 79)
