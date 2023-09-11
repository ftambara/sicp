#lang racket

(require rackunit)


(define (memq item sequence)
  (cond ((null? sequence) false)
        ((eq? item (car sequence)) sequence)
        (else (memq item (cdr sequence)))))

(check-false (memq 'apple '(banana pear orange)))
(check-false (memq 'apple '(bananas pears apples)))
(check-equal? (memq 'apple '(banana pear apple)) '(apple))
(check-equal?
 (memq 'apple '(banana (apple peach) apple pear)) '(apple pear))

