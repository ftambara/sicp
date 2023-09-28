#lang racket

(require rackunit)

(provide make-tree
         entry
         left-branch
         right-branch
         make-leave
         tree->list)


;; From the book

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define calls-1 0)
(define calls-2 0)

(define (count-call-1)
  (set! calls-1 (+ calls-1 1)))
(define (count-call-2)
  (set! calls-2 (+ calls-2 1)))
(define (reset-counters)
  (set! calls-1 0)
  (set! calls-2 0))

(define (tree->list-1 tree)
  (count-call-1)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (count-call-2)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;; Solution
(define (make-leave num)
  (make-tree num '() '()))

(define tree1
  (make-tree 7
             (make-tree 3
                        (make-leave 1)
                        (make-leave 5))
             (make-tree 9
                        '()
                        (make-leave 11))))

(check-equal? (tree->list-1 tree1) '(1 3 5 7 9 11))
; (printf "tree->list-1 calls: ~a\n" calls-1)
; (reset-counters)
(check-equal? (tree->list-2 tree1) '(1 3 5 7 9 11))
; (printf "tree->list-2 calls: ~a\n" calls-2)
; (reset-counters)

(define tree2
  (make-tree 3
             (make-leave 1)
             (make-tree 7
                        (make-leave 5)
                        (make-tree 9
                                   '()
                                   (make-leave 11)))))

(check-equal? (tree->list-1 tree2) '(1 3 5 7 9 11))
; (printf "tree->list-1 calls: ~a\n" calls-1)
; (reset-counters)
(check-equal? (tree->list-2 tree2) '(1 3 5 7 9 11))
; (printf "tree->list-2 calls: ~a\n" calls-2)
; (reset-counters)

(define tree3
  (make-tree 5
             (make-tree 3
                        (make-leave 1)
                        '())
             (make-tree 9
                        (make-leave 7)
                        (make-leave 11))))

(check-equal? (tree->list-1 tree3) '(1 3 5 7 9 11))
; (printf "tree->list-1 calls: ~a\n" calls-1)
; (reset-counters)
(check-equal? (tree->list-2 tree3) '(1 3 5 7 9 11))
; (printf "tree->list-2 calls: ~a\n" calls-2)
; (reset-counters)

;; a.
;; Despite my confused intuition about these procedures,
;; both implementations seem to produce an ordered list
;; out of any binary tree.

;; b.
;; After adding the counters, it seems that the number of calls are
;; equal for both procedures (13 in all cases). Order of growth is
;; O(n), since both procedures traverse each node once, including
;; empty leaves.
;; The first implementation has an advantage in space, since it
;; doesn't copy the accumulated list for every left-branch step.

(define tree->list tree->list-1)
