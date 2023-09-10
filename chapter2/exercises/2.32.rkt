#lang racket

(require rackunit)


(define (subsets set_)
  (if (null? set_)
      (list null)
      (let ((rest (subsets (cdr set_))))
        (append rest (map (lambda (sub) (cons (car set_) sub)) rest)))))

(check-equal?
 (subsets '(1 2 3))
 '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))

; The followingn visualization helped me understand
; why this method works:

; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))

; Is the same as:

; (append '( ()   (3)   (2)    (2 3))
;         '((1) (1 3) (1 2) (1 2 3)))

; For a given set, all the combinations possible result
; from all the combinations of the set without the first
; item, plus the same combinations with the first element
; added to the beginning (this only works with same-order
; combinations)
; Using recursion and the empty list as base case, it's
; possible to solve the problem for sets of any size.
