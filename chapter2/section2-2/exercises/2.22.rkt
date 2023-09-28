#lang racket

(provide reverse-square-list reverse-square-list-alt square-list-iter)

(require "../utils.rkt")

(define (square x)
  (* x x))

(define (reverse-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

; This implementation builds up answer in reverse
; order by taking the leftmost element of 'things'
; and stacking it on the left of 'answer', effectively
; building a sort of LIFO stack.

(define (reverse-square-list-alt items)
  (define (iter things answer)
    (echo answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

; The links between these pairs are not done in the standard
; list way. Instead of putting values on the car's and linking
; back from the cdr's, here the cars point to the rest of the
; structure, while the cdr's hold the values.
; Furthermore, the null value is placed at the front of the
; list, which is again non-standard.

; A solution to solve it iteratively would be to start the iterative
; chain with (reverse items), which can also be implemented
; iteratively (see 2.18.rkt).

(define (square-list-iter items)
  (define (iter things answer)
    (echo answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter (reverse items) null))

; > (square-list-iter '(1 2 3 5))
; ()
; (25)
; (9 25)
; (4 9 25)
; (1 4 9 25)
; '(1 4 9 25)
