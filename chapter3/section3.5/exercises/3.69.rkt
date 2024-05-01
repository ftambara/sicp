#lang racket

;; Write a procedure triples that takes three infinite streams, S, T, and U,
;; and produces the stream of triples (Si , Tj , Uk) such that i ≤ j ≤ k.

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

;; Use triples to generate the stream of all Pythagorean triples of positive
;; integers, i.e., the triples (i, j, k) such that i ≤ j and i^2 + j^2 = k^2.

(define pythagorean-triples
  (stream-filter
    (lambda (triple) (= (+ (expt (first triple) 2)
                           (expt (second triple) 2))
                        (expt (third triple) 2)))
    (triples integers integers integers)))

(stream-display (stream-take pythagorean-triples 4))
;; (3 4 5)
;; (6 8 10)
;; (5 12 13)
;; (9 12 15)

;; This solution is extremely slow. One attempt to improve it could be
;; to compute pairs of Tj, Uk only once, and prepend all Si from 0 to j
;; for each entry.
