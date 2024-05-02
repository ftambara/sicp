#lang racket

;; Numbers that can be expressed as the sum of two cubes in more than one way
;; are sometimes called Ramanujan numbers, in honor of the mathematician
;; Srinivasa Ramanujan. Ordered streams of pairs provide an elegant solution to
;; the problem of computing these numbers. To ﬁnd a number that can be written
;; as the sum of two cubes in two different ways, we need only generate the
;; stream of pairs of integers (i, j) weighted according to the sum i3 + j3
;; (see Exercise 3.70), then search the stream for two consecutive pairs with
;; the same weight. Write a procedure to generate the Ramanujan numbers. The
;; first such number is 1,729. What are the next five?

(require
  "../../modules/streams.rkt"
  rackunit)

(define (sum-of-cubes p)
  (+ (expt (car p) 3) (expt (cadr p) 3)))

(define (find-duplicates stream weight)
  (define (loop stream)
    (if (or (stream-empty? stream) (stream-empty? (stream-rest stream)))
      empty-stream
      (let ([this-pair (stream-first stream)]
            [rest-str (stream-rest stream)])
        (if (= (weight this-pair) (weight (stream-first rest-str)))
          (stream-cons this-pair (loop rest-str))
          (loop rest-str)))))
  (loop stream))

(define ramanujan-seq
  (find-duplicates (pairs-weighted integers integers sum-of-cubes)
                   sum-of-cubes))

(check-eq? (sum-of-cubes (stream-first ramanujan-seq)) 1729)
