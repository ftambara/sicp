#lang racket

(require
  "../../modules/streams.rkt"
  "3.59.rkt"
  rackunit)

(define (mul-series s1 s2)
  (stream-cons
    (* (stream-first s1) (stream-first s2))
    (add-streams (scale-stream (stream-rest s2) (stream-first s1))
                 (mul-series (stream-rest s1) s2))))

(define s (add-streams (mul-series cosine-series cosine-series)
                       (mul-series sine-series sine-series)))
(check-equal? (stream->list (stream-take s 4)) '(1 0 0 0))
