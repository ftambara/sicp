#lang racket

(provide last-pair)

(define (last-pair list_)
  (cond ((null? list_) (error "list is empty"))
        ((null? (cdr list_)) (car list_))
        (else (last-pair (cdr list_)))))
