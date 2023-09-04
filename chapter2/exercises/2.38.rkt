#lang racket

(require rackunit)

(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))


(check-equal? 3/2 (fold-right / 1 (list 1 2 3)))
(check-equal? 1/6 (fold-left / 1 (list 1 2 3)))
(check-equal? '(1 (2 (3 ()))) (fold-right list null (list 1 2 3)))
(check-equal? '(((() 1) 2) 3) (fold-left list null (list 1 2 3)))

(check-equal? 3/8 (fold-right / 1 (list 1 2 3 4)))
(check-equal? 1/24 (fold-left / 1 (list 1 2 3 4)))

; If op is commutative, both fold methods will
; return the same result

(check-equal?
    (fold-right + 0 '(1 2 3 4))
    (fold-left + 0 '(1 2 3 4)))

(check-equal?
    (fold-right * 1 '(1 2 3 4))
    (fold-left * 1 '(1 2 3 4)))
