#lang racket

(require
  compatibility/mlist
  rackunit
  "3.13.rkt")

(define (is-shallow-mcyclic? mlst)
  ;; Tortoise and hare algorithm
  (define (next-tortoise t)
    (mcdr t))
  (define (next-hare h)
    (if (mpair? (mcdr h))
        (mcdr (mcdr h))
        null))
  (define (loop tortoise hare)
    (cond ((null? hare) false)
          ((eq? tortoise hare) true)
          (else (loop (next-tortoise tortoise)
                      (next-hare hare)))))
  (loop (next-tortoise mlst) (next-hare mlst)))

(define l (mlist 1 2 3))
(check-false (is-shallow-mcyclic? l))
(define c1 (make-cycle l))
(check-true (is-shallow-mcyclic? c1))
(define c2 (mlist 1 2 c1 4))
(check-false (is-shallow-mcyclic? c2))
(define short (mlist 1))
(check-false (is-shallow-mcyclic? short))
