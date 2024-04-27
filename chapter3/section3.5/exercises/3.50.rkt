#lang racket

(require rackunit)

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
    empty-stream
    (stream-cons
      (apply proc (map stream-first argstreams))
      (apply stream-map proc (map stream-rest argstreams)))))

(define (stream-enum start end)
  (if (>= start end)
    empty-stream
    (stream-cons start
                 (stream-enum (add1 start) end))))

(define enum1 (stream-enum 1 5))
(define enum2 (stream-enum 11 15))
(define enum3 (stream-enum 21 25))

(define mapped (stream-map + enum1 enum2 enum3))
(check-eq? (stream-first mapped) 33)
(check-eq? (stream-first (stream-rest mapped)) 36)
