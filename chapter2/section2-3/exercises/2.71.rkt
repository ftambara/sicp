#lang racket

(require "2.67.rkt"
         "2.69.rkt"
         rackunit)

;; 0 + 1 = 2 - 1 = 1
;; 1 + 2 = 4 - 1 = 3
;; 3 + 4 = 8 - 1 = 7
;; ...
;; sum(2^k; k=0; k=n-1) = 2^n - 1

;; The sum of the n-1 smallest nodes of the tree is always smaller
;; than the nth node, therefore the shape of the tree is predictable.

;;                  2^n - 1
;;                  /     \
;;             2^(n-2)   2^(n-1)
;;          ...
;;          15
;;         /  \
;;       7     8
;;     /  \
;;   3    4
;;  / \
;; 1   2

;; The most frequent symbol is always encoded by one bit
;; The least frequent symbol is encoded by n-1 bits.

(define (code-tree-depth tree)
  (if (leaf? tree)
      0
      (max (+ 1 (code-tree-depth (left-branch tree)))
           (+ 1 (code-tree-depth (right-branch tree))))))

(check-equal? (code-tree-depth
               (generate-huffman-tree '((A 4) (B 2) (C 1))))
              2)

(check-equal? (code-tree-depth
               (generate-huffman-tree '((A 8) (B 4) (C 2) (D 1))))
              3)

(check-equal? (code-tree-depth
               (generate-huffman-tree
                '((A 16) (B 8) (C 4) (D 2) (E 1))))
              4)
