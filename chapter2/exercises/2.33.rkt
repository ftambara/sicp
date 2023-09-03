#lang racket

(require "../utils.rkt")


(define (map p sequence)
    (display "Using custom map implementation\n")
    (accumulate (lambda (x y) (cons (p x) y)) null sequence))

; This is counter-intuitive to the way
; I always thought accumulate worked
(define (append seq1 seq2)
    (display "Using custom append implementation\n")
    (accumulate cons seq2 seq1))

(define (length sequence)
    (display "Using custom length implementation\n")
    (accumulate (lambda (elem total) (add1 total)) 0 sequence))
