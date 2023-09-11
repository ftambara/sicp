#lang racket

(require rackunit)


; returns a list whose elements are all the leaves of the
; tree arranged in left-to-right order
(define (fringe tree)
  (cond ((null? tree) tree)
        ((list? tree)
         (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))

(define x (list (list 1 2) (list 3 4)))
(check-equal? (fringe x) '(1 2 3 4))
(check-equal? (fringe (list x x)) '(1 2 3 4 1 2 3 4))
