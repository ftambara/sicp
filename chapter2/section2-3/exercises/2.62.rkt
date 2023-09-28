#lang racket

(require rackunit)

(provide intersection-ord-set
         union-ord-set)


(define (union-ord-set ord-set1 ord-set2)
  (cond ((null? ord-set1) ord-set2)
        ((null? ord-set2) ord-set1)
        ((< (car ord-set1) (car ord-set2))
         (cons (car ord-set1)
               (union-ord-set (cdr ord-set1) ord-set2)))
        ((> (car ord-set1) (car ord-set2))
         (cons (car ord-set2)
               (union-ord-set ord-set1 (cdr ord-set2))))
        ((= (car ord-set1) (car ord-set2))
         (cons (car ord-set1)
               (union-ord-set (cdr ord-set1) (cdr ord-set2))))
        (else (error "Unexpected case"))))


(check-equal? (union-ord-set '(1 3 5) '(2 3 4 5)) '(1 2 3 4 5))
(check-equal? (union-ord-set '(1 2 3) '(4 5)) '(1 2 3 4 5))
(check-equal? (union-ord-set '(4 5) '(1 2 3)) '(1 2 3 4 5))

;; From the book
(define (intersection-ord-set ord-set1 ord-set2)
  (cond ((or (null? ord-set1) (null? ord-set2)) '())
        ((= (car ord-set1) (car ord-set2))
         (cons (car ord-set1)
               (intersection-ord-set (cdr ord-set1) ord-set2)))
        ((> (car ord-set1) (car ord-set2))
         (intersection-ord-set ord-set1 (cdr ord-set2)))
        ((< (car ord-set1) (car ord-set2))
         (intersection-ord-set (cdr ord-set1) ord-set2))))

(check-equal? (intersection-ord-set '(1 3 5 6 9) '(2 3 4 6 9)) '(3 6 9))
