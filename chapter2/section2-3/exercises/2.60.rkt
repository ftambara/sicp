#lang racket

(require rackunit)


(define (element-of-set? item set)
  (if (null? set)
      false
      (or (equal? item (car set))
          (element-of-set? item (cdr set)))))

(define (adjoin-set item set)
  (cons item set))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(check-equal? (intersection-set '(1 2 3 4) '(1 2 1)) '(1 2))
(check-equal? (intersection-set '(1 2 3 2 4) '(1 2 1)) '(1 2 2))
(check-equal? (union-set '(1 2 3 4) '(1 2 1)) '(4 3 2 1 1 2 1))

;; element-of-set? still grows like O(n), while adjoin-set is
;; now O(1). As consequence, intersection-set is still O(n^2), now
;; with a larger n since the list may contain repeated elements,
;; while union-set is O(n) instead of O(n^2).
