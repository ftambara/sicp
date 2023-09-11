#lang racket

(provide square-list square-list-map)

(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

; > (square-list '(1 3 2))
; '(1 9 4)
; > (square-list-map '(1 2 5))
; '(1 4 25)
