#lang racket

(require "2.67.rkt"
         rackunit)


;; From the book

(define (adjoin-set-weight x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set-weight x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set-weight (make-leaf (car pair)   ; symbol
                                      (cadr pair)) ; frequency
                           (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


;; Solution

(define (successive-merge nodes)
  (let ((nodes_n (length nodes)))
    (cond ((= nodes_n 1) (car nodes))
          ((> nodes_n 1)
           (let* ((smallest (take nodes 2))
                  (new-node
                   (make-code-tree (car smallest) (cadr smallest)))
                  (rest (drop nodes 2)))
             (successive-merge (adjoin-set-weight new-node rest))))
          (else "Cannot merge emtpy node set"))))


;; Tests

(check-equal? (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
              (make-code-tree
               (make-leaf 'A 4)
               (make-code-tree
                (make-leaf 'B 2)
                (make-code-tree
                 (make-leaf 'D 1)
                 (make-leaf 'C 1)))))
