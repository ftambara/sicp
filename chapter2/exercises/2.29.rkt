#lang racket

(require rackunit)


; (define (make-mobile left right)
;     (list left right))
(define (make-mobile left right) (cons left right))

; (define (make-branch length structure)
;     (list length structure))
(define (make-branch length structure)
    (cons length structure))
; Note: Implementations changes according to item d


; a. Selectors
(define (left-branch mobile)
    (car mobile))

; (define (right-branch mobile)
;     (cadr mobile))
(define (right-branch mobile)
    (cdr mobile))

(define (branch-length branch)
    (car branch))

; (define (branch-structure branch)
;     (cadr branch))
(define (branch-structure branch)
    (cdr branch))

(define left
    (make-branch 2 5))
(define right-sub-l
    (make-branch 3 1))
(define right-sub-r
    (make-branch 1 3))
(define right-mob
    (make-mobile right-sub-l right-sub-r))
(define right
    (make-branch 3 right-mob))
(define mob
    (make-mobile left right))

(check-equal? (left-branch mob) left)
(check-equal? (right-branch mob) right)
(check-equal? (branch-length left) 2)
(check-equal? (branch-structure left) 5)
(check-equal? (branch-structure right) right-mob)

; b. Total weight
(define (mobile? obj)
    (and (pair? obj)
         (branch? (car obj))
         (branch? (car obj))))

; (define (branch? obj)
;     (and (pair? obj)
;          (number? (car obj))
;          (or (number? (cadr obj))
;               (mobile? (cadr obj)))))
(define (branch? obj)
    (and (pair? obj)
         (number? (car obj))
         (or (number? (cdr obj))
              (mobile? (cdr obj)))))


(check-true (mobile? mob))
(check-false (mobile? left))
(check-true (branch? left))
(check-true (branch? right))

(define (branch-weight branch)
    (let ((structure (branch-structure branch)))
            (if (mobile? structure)
                (total-weight structure)
                structure)))

(define (total-weight mobile)
    (if (mobile? mobile)
        (+ (branch-weight (left-branch mobile))
            (branch-weight (right-branch mobile)))
        0))

(check-equal? (total-weight mob) 9)
(check-equal? (total-weight right-mob) 4)

; c. Balanced predicate
(define (mobile-torque mobile)
    (define (branch-torque branch)
        (* (branch-length branch)
           (branch-weight branch)))
    (if (mobile? mobile)
        (- (branch-torque (right-branch mobile))
            (branch-torque (left-branch mobile)))
        0))

(define (balanced? mobile)
    (= (mobile-torque mobile) 0))

(check-false (balanced? mob))

(define balanced-mobile
    (make-mobile (make-branch 3 6)
                 (make-branch 2 9)))

(check-true (balanced? balanced-mobile))

; d. Change of representation
; Changes written below each original 
; definition in the top half of the file
