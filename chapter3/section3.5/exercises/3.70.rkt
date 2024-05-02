#lang racket

;; It would be nice to be able to generate streams in which the pairs appear in
;; some useful order, rather than in the order that results from an ad hoc
;; interleaving process. We can use a technique similar to the merge procedure
;; of Exercise 3.56, if we deﬁne a way to say that one pair of integers is
;; “less than” another. One way to do this is to define a “weighting function”
;; W(i, j) and stipulate that (i1 , j1) is less than (i2, j2) if
;; W(i1, j1) < W(i2, j2). Write a procedure merge-weighted that is like merge,
;; except that merge-weighted takes an additional argument weight, which is a
;; procedure that computes the weight of a pair, and is used to determine the
;; order in which elements should appear in the resulting merged stream.

(require "../../modules/streams.rkt")

(define (merge-weighted pairs1 pairs2 weight)
  (cond [(stream-empty? pairs1) pairs2]
        [(stream-empty? pairs2) pairs1]
        [else
          (let ([p1 (stream-first pairs1)]
                [p1cdr (stream-rest pairs1)]
                [p2 (stream-first pairs2)]
                [p2cdr (stream-rest pairs2)])
            (if (< (weight p1) (weight p2))
              (stream-cons p1 (merge-weighted p1cdr pairs2 weight))
              (stream-cons p2 (merge-weighted pairs1 p2cdr weight))))]))

;; Using this, generalize pairs to a procedure weighted-pairs that takes two
;; streams, together with a procedure that computes a weighting function, and
;; generates the stream of pairs, ordered according to weight. Use your
;; procedure to generate
;;  a. the stream of all pairs of positive integers (i, j) with i ≤ j ordered
;;      according to the sum i + j,
;;  b. the stream of all pairs of positive integers (i, j) with i ≤ j, where
;;      neither i nor j is divisible by 2, 3, or 5, and the pairs are ordered
;;      according to the sum 2i + 3j + 5ij.

;; Important point:
;; We will require that the weighting function be such that the weight of a
;; pair increases as we move out along a row or down along a column of the
;; array of pairs.
;; This means that for any given weighted-pairs instance, we the pair formed
;; by the first elements of s and t are always the lowest, so we know which
;; pair to place first.

(define (weighted-pairs s t weight)
  (stream-cons
    (list (stream-first s) (stream-first t))
    (merge-weighted
      (stream-map (lambda (ti) (list (stream-first s) ti)) (stream-rest t))
      (weighted-pairs (stream-rest s) (stream-rest t) weight)
      weight)))

(define (weight-a p) (+ (car p) (cadr p)))

(stream-display (stream-take (weighted-pairs integers integers weight-a) 10))
;; (1 1)
;; (1 2)
;; (2 2)
;; (1 3)
;; (2 3)
;; (1 4)
;; (3 3)
;; (2 4)
;; (1 5)
;; (3 4)

(define (divisible? a b)
  (= (remainder a b) 0))
(define (keep-b? x)
  (and (not (divisible? x 2))
       (not (divisible? x 3))
       (not (divisible? x 5))))

(define (weight-b p)
  (let ([i (car p)] [j (cadr p)])
    (+ (* 2 i) (* 3 j) (* 5 i j))))

(define integers-b (stream-filter keep-b? integers))
(stream-display (stream-take (weighted-pairs integers-b integers-b weight-b) 10))

;; (1 1)
;; (1 7)
;; (1 11)
;; (1 13)
;; (1 17)
;; (1 19)
;; (1 23)
;; (1 29)
;; (1 31)
;; (7 7)
