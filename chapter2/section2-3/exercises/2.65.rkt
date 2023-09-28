#lang racket

(require "2.62.rkt"
         "2.63.rkt"
         "2.64.rkt"
         rackunit)


;; give Θ(n) implementations of union-set and
;; intersection-set for sets implemented as (balanced) bi-nary trees.

;; Assume sets are implemented as ordered lists.
;; Restrict elements to numbers to make comparisons easier.
(define (union-set-balanced set1 set2)
  (list->tree (union-ord-set (tree->list set1) (tree->list set2))))

(define (intersection-set-balanced set1 set2)
  (list->tree (intersection-ord-set (tree->list set1)
                                    (tree->list set2))))

(define set1
  (make-tree 5
             (make-tree 3
                        (make-leave 2)
                        (make-leave 4))
             (make-tree 7
                        (make-leave 6)
                        (make-leave 8))))

(define set2
  (make-tree 6
             (make-tree 3
                        (make-leave 2)
                        (make-leave 4))
             (make-tree 8
                        (make-leave 7)
                        (make-leave 9))))

(check-equal? (tree->list (union-set-balanced set1 set2))
              '(2 3 4 5 6 7 8 9))

(check-equal? (tree->list (intersection-set-balanced set1 set2))
              '(2 3 4 6 7 8))
