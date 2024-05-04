#lang racket

;; Eva Lu Ator has a criticism of Louis’s approach in Exercise 3.75. The
;; program he wrote is not modular, because it intermixes the operation of
;; smoothing with the zero-crossing extraction. For example, the extractor
;; should not have to be changed if Alyssa finds a better way to condition her
;; input signal. Help Louis by writing a procedure smooth that takes a stream
;; as input and produces a stream in which each element is the average of two
;; successive input stream elements. Then use smooth as a component to
;; implement the zero-crossing detector in a more modular style.

(require
  "../../modules/streams.rkt")

(define (average a b) (/ (+ a b) 2))
(define (smooth input-signal)
  (stream-map-all average
                  (stream-rest input-signal)
                  input-signal))

(define (sign-change-detector new last)
  (cond [(and (positive? new) (negative? last)) 1]
        [(and (negative? new) (positive? last)) -1]
        [else 0]))

(define (make-zero-crossings input-stream last-value)
  (stream-cons
    (sign-change-detector
      (stream-car input-stream)
      last-value)
    (make-zero-crossings
      (stream-cdr input-stream)
      (stream-car input-stream))))

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(define zero-crossings (make-zero-crossings (smooth sense-data) 0))

(stream->list (stream-take zero-crossings 12))
;; '(0 0 0 0 0 0 -1 0 0 0 0 1)
