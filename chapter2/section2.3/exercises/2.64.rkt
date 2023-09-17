#lang racket

(require rackunit)


;; From the book, with minor changes

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (let* ((result (partial-tree elements (length elements)))
         (tree (car result))
         (rest (cdr result)))
    (if (null? rest)
        tree
        (error "list->tree: " rest " not empty"))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      ;; Put floor((n - 1) / 2) elements on the left branch
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             ;; All other items (minus one for this node)
             ;; go on the right branch
             (right-size (- n (+ left-size 1)))
             ;; Reserve one element for this node
             (this-entry (car non-left-elts))
             ;; Put all the others on the right branch
             (right-result
              (partial-tree (cdr non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry
                         left-tree
                         right-tree)
              remaining-elts))))

;; a.
;; The partial-tree procedure works as follows:
;; - Of all the elements in the elts list, build the left branch with
;;  the first floor((n - 1) / 2) elements
;; - From the rest of those elements, reserve the first for this node
;; - Place (n - floor((n - 1) / 2) + 1) elements on the right branch
;; - Build a tree from the left branch, node, and right branch
;; - Return all elements the newly-built tree, and all elements that
;;  didn't fit as rest
;; - As base case, return an empty tree and all elements from the
;;  given list as rest when n = 0
;;
;; The solution is then recursively defined. A few insights that
;; helped me understand where the action happens:
;; - The first non-empty node that gets built is returned as a branch
;;  later, which get's incorporated in the higher level tree with the
;;  make-tree call at the end of the procedure.
;; - Only smaller copies of the original elts list passes around as
;;  rest, by virtue of carefully selected 'slots' for the
;;  sub-branches.
;;
;; High-level example: Convert '(1 3 5 7 9 11) to a tree
;;
;; The list contains 6 elements. Split into 3 parts:
;; - Left branch: '(1 3)
;; - This node: 5
;; - Right branch: '(7 9 11)
;;
;; Recursively, build the left branch:
;; - Left branch: '()
;; - This node: 1
;; - Right branch: '(3)
;;
;; Build the right branch:
;; - Left branch: '(7)
;; - This node: 9
;; - Right branch: '(11)

;; Tree for (list->tree '(1 3 5 7 9 11))
(list->tree '(1 3 5 7 9 11))

;; Diagram
;;
;;       5
;;     /   \
;;   1       9
;;    \     / \
;;     3   7   11

