#lang racket

(provide make-segment
         start-segment
         end-segment)


(define (make-segment vector-start vector-end)
  (cons vector-start vector-end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
