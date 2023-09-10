#lang racket

(require
  "2.38.rkt"
  rackunit)


(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))
(define (reverse-r sequence)
  (fold-right
   (lambda (x y) (if (null? y) (list x) (append y (list x))))
   null
   sequence))

(define seq '(1 2 3 4 5))
(define rev-seq '(5 4 3 2 1))

(check-equal? rev-seq (reverse-l seq) "reverse-l")
(check-equal? rev-seq (reverse-r seq) "reverse-r")
