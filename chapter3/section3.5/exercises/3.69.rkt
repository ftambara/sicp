#lang racket

;; Write a procedure triples that takes three infinite streams, S, T, and U,
;; and produces the stream of triples (Si , Tj , Uk) such that i ≤ j ≤ k.
;; Use triples to generate the stream of all Pythagorean triples of positive
;; integers, i.e., the triples (i, j, k) such that i ≤ j and i^2 + j^2 = k^2.

(require "../../modules/streams.rkt")

(define (triples s t u)
  (let* ([sf (stream-first s)]
        [pair-str (pairs t u)])
    (stream-cons
      (cons sf (stream-first pair-str))
      (interleave
        (stream-map (lambda (pair) (cons sf pair)) (stream-rest pair-str))
        (triples (stream-rest s) (stream-rest t) (stream-rest u))))))

(define integers (stream-cons 1 (stream-map add1 integers)))

(stream-display (stream-take (triples integers integers integers) 10))
;; (1 1 1)
;; (1 1 2)
;; (2 2 2)
;; (1 2 2)
;; (2 2 3)
;; (1 1 3)
;; (3 3 3)
;; (1 2 3)
;; (2 3 3)
;; (1 1 4)
