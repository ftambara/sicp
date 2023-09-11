#lang racket

(require rackunit)

;; From the book
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; Solutions
(check-equal? (list 'a 'b 'c) '(a b c))
(check-equal? (list (list 'george)) '((george)))
(check-equal? (cdr '((x1 x2) (y1 y2))) '((y1 y2)))
(check-equal? (cadr '((x1 x2) (y1 y2))) '(y1 y2))
(check-equal? (pair? (car '(a short list))) false)
(check-equal? (memq 'red '((red shoes) (blue socks))) false)
(check-equal? (memq 'red '(red shoes blue socks)) 'red)
