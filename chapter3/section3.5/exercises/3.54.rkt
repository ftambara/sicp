#lang racket

(require
  "../../modules/streams.rkt"
  rackunit)

(define (mul-streams s1 s2) (stream-map-all * s1 s2))

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))

;; the stream whose n-th element (counting from 0) is n + 1 factorial

(define factorial-stream
  (stream-cons
    1
    (mul-streams factorial-stream
                 (stream-rest integers))))

(check-eq? (stream-ref factorial-stream 0) 1)
(check-eq? (stream-ref factorial-stream 1) 2)
(check-eq? (stream-ref factorial-stream 2) 6)
(check-eq? (stream-ref factorial-stream 3) 24)
