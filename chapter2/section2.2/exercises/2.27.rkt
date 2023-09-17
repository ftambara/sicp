#lang racket

(require rackunit)


(define (deep-reverse sequence)
  (define (iter result remain)
    (cond ((null? remain) result)
          ((list? (car remain))
           (iter (cons (deep-reverse (car remain)) result) (cdr remain)))
          (else (iter (cons (car remain) result) (cdr remain)))))
  (iter '() sequence))


(define x (list (list 1 2) (list 3 4)))

(check-equal? x '((1 2) (3 4)) "Setup")
(check-equal? (reverse x) '((3 4) (1 2)) "Shallow reverse")
(check-equal? (deep-reverse x) '((4 3) (2 1)) "Deep reverse")
