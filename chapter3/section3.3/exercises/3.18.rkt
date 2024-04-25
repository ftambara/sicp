#lang racket

(require
  rackunit
  "3.12.rkt"
  "3.13.rkt")

(define (is-shallow-mcyclic? lst)
  (let ((memo '()))
    (define (loop rest)
      (if (empty? rest)
        false
        (if (memq (mcar rest) memo)
          true
          (begin
            (set! memo (cons (mcar rest) memo))
            (loop (mcdr rest))))))
    (loop lst)))

(define l (mlist 1 2 3))
(check-false (is-shallow-mcyclic? l))
(define c1 (make-cycle l))
(check-true (is-shallow-mcyclic? c1))
(define c2 (mlist 1 2 c1 4))
(check-false (is-shallow-mcyclic? c2))
