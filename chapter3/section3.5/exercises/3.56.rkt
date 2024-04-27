#lang racket

(require
  "../../modules/streams.rkt"
  rackunit)

(define (merge s1 s2)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else
          (let ([s1car (stream-car s1)]
                [s2car (stream-car s2)])
            (cond [(< s1car s2car)
                   (stream-cons s1car
                                (merge (stream-cdr s1) s2))]
                  [(> s1car s2car)
                   (stream-cons s2car
                                (merge s1 (stream-cdr s2)))]
                  [else
                    (stream-cons s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))]))]))
(define hamming235
  (stream-cons
    1
    (merge (merge (scale-stream hamming235 2)
                  (scale-stream hamming235 3))
           (scale-stream hamming235 5))))

(check-eq? (stream-ref hamming235 0) 1)
(check-eq? (stream-ref hamming235 1) 2)
(check-eq? (stream-ref hamming235 2) 3)
(check-eq? (stream-ref hamming235 3) 4)
(check-eq? (stream-ref hamming235 4) 5)
(check-eq? (stream-ref hamming235 5) 6)
(check-eq? (stream-ref hamming235 6) 8)
(check-eq? (stream-ref hamming235 9) 12)
(check-eq? (stream-ref hamming235 10) 15)
