#lang racket

(provide reverse-list)

(define (reverse-list list_)
    (define (iter reversed remain)
        (if (null? remain)
            reversed
            (iter (cons (car remain) reversed) (cdr remain))))
    (iter '() list_))