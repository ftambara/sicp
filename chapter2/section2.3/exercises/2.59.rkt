#lang racket

(require rackunit)


;; From the book
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


;; Solution
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))

; The 'unit test' breaks the abstraction layer, it should
; at least compare the sets using a special selector function.
; Also, sets should be built using a constructor.
(check-equal? (union-set '(1 2 3) '(3 4 5)) '(2 1 3 4 5))
