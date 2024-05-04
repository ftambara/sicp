#lang racket

(require "../../modules/streams.rkt")

(define (sign-change-detector new last)
  (cond [(and (positive? new) (negative? last)) 1]
        [(and (negative? new) (positive? last)) -1]
        [else 0]))

;; Unfortunately, Alyssa’s zero-crossing detector in Exercise 3.74 proves to be
;; insufficient, because the noisy signal from the sensor leads to spurious
;; zero crossings. Lem E. Tweakit, a hardware specialist, suggests that Alyssa
;; smooth the signal to filter out the noise before extracting the zero
;; crossings. Alyssa takes his advice and decides to extract the zero crossings
;; from the signal constructed by averaging each value of the sense data with
;; the previous value. She explains the problem to her assistant, Louis
;; Reasoner, who attempts to implement the idea, altering Alyssa’s program as
;; follows:
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (stream-cons
      (sign-change-detector avpt last-value)
      (make-zero-crossings
        (stream-cdr input-stream) avpt))))
;; This does not correctly implement Alyssa’s plan. Find the bug that Louis has
;; installed and fix it without changing the structure of the program. (Hint:
;; You will need to increase the number of arguments to make-zero-crossings.) 

;; This implementation averages the current value with the previous average.
;; The averages should simply be between the current and previous values of
;; input-stream.
(define (fixed-make-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (stream-cons
      (sign-change-detector avpt last-avg)
      (make-zero-crossings
        (stream-cdr input-stream) (stream-car input-stream) avpt))))


;; Why not make a filter-noise procedure?
(define (average a b) (/ (+ a b) 2))
(define (filter-noise input-signal)
  (stream-map-all average
                  input-signal
                  (stream-cons (stream-first input-signal)
                               input-signal)))

(define sense-data (stream 1 2 1.5 1 0.1 -0.1 0.2 -2 -3 -0.5 3 4))

(map exact->inexact (stream->list (filter-noise sense-data)))
;; '(1.0 1.5 1.75 1.25 0.55 0.0 0.05 -0.9 -2.5 -1.75 1.25 3.5)

(define (lu-make-zero-crossings input-signal)
  (stream-map-all sign-change-detector
                  input-signal
                  (stream-cons (stream-first input-signal)
                               input-signal)))

(map exact->inexact (stream->list (lu-make-zero-crossings sense-data)))
;; '(0.0 0.0 0.0 0.0 0.0 -1.0 1.0 -1.0 0.0 0.0 1.0 0.0)
(map exact->inexact (stream->list (lu-make-zero-crossings (filter-noise sense-data))))
;; '(0.0 0.0 0.0 0.0 0.0 0.0 0.0 -1.0 0.0 0.0 1.0 0.0)
