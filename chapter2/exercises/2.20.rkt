#lang racket

(provide same-parity)

(define (echo x)
    (display x)
    (newline)
    x)
(define (same-parity ref . integers)
    (define (include? num)
        (or (echo (and (even? num) (even? ref)))
            (and (odd? num) (odd? ref))))
    (cond ((null? integers) ref)
          ((include? (car integers))
            (cons (car integers) (same-parity ref (echo (cdr integers)))))
          (else (same-parity (cdr integers)))))

(same-parity 1 2 3 4 5 6 7); => (1 3 5 7)
(same-parity 2 3 4 5 6 7); => (2 4 6)
