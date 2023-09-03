#lang racket


(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) null sequence))

; This is counter-intuitive to the way
; I always thought accumulate worked
(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (elem total) (add1 total)) 0 sequence))
