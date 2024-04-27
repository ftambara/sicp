#lang racket

(require
  "../../modules/streams.rkt"
  rackunit)

(define (partial-sums s)
  (stream-cons (stream-first s)
               (add-streams (partial-sums s)
                            (stream-rest s))))

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))

(define sum (partial-sums integers))
(check-eq? (stream-ref sum 0) 1)
(check-eq? (stream-ref sum 1) 3)
(check-eq? (stream-ref sum 2) 6)
(check-eq? (stream-ref sum 3) 10)
