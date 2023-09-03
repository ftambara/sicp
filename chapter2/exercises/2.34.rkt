#lang racket

(require rackunit)

; From the book
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
    (accumulate
        (lambda (elem accu) (+ elem (* accu x)))
        0
        coefficient-sequence))

(check-equal? (horner-eval 2 (list 1 3 0 5 0 1)) 79)
