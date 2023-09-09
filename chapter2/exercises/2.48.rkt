#lang racket


(define (make-segment vector-start vector-end)
    (cons vector-start vector-end))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))
