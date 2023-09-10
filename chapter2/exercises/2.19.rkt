#lang racket

(provide cc us-coins uk-coins)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(cc 100 us-coins); => 292

; The order of the list of coin values does not
; matter, since coins are dealt with one at a time
; for each node of the recursion tree.

; Example
(cc 100 (list 1 2 3 4 5)); => 46262
(cc 100 (list 5 3 1 2 4)); => 46262
