#lang racket

(require rackunit)


(define (adjoin-ord-set elem ord-set)
  (cond ((null? ord-set) (list elem))
        ((> elem (car ord-set))
         (cons (car ord-set) (adjoin-ord-set elem (cdr ord-set))))
        ((< elem (car ord-set)) (cons elem ord-set))
        (else ord-set)))


(check-equal? (adjoin-ord-set 3 '(1 2 4 5)) '(1 2 3 4 5))
(check-equal? (adjoin-ord-set 3 '(1 2 3)) '(1 2 3))
(check-equal? (adjoin-ord-set 3 '(4 5)) '(3 4 5))

;; Worst case, O(n); n: number of elements of ord-set
(check-equal? (adjoin-ord-set 3 '(1 2)) '(1 2 3))
