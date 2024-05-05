#lang racket

;; Redo Exercise 3.5 on Monte Carlo integration in terms of streams. The stream
;; version of estimate-integral will not have an argument telling how many
;; trials to perform. Instead, it will produce a stream of estimates based on
;; successively more trials.

(require
  "../../modules/streams.rkt"
  rackunit)

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-rest experiment-stream) passed failed)))
  (if (stream-first experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

(define (estimate-integral pred x1 x2 y1 y2)
  (scale-stream (monte-carlo
                  (stream-map
                    pred
                    (points-inside-rectangle x1 x2 y1 y2))
                  0
                  0)
                (rectangle-area x1 x2 y1 y2)))

(define (points-inside-rectangle x1 x2 y1 y2)
  (stream-cons
    (cons (random-in-range x1 x2) (random-in-range y1 y2))
    (points-inside-rectangle x1 x2 y1 y2)))

(define (random-in-range min max)
  (+ (* (random) (- max min)) min))

(define (rectangle-area x1 x2 y1 y2)
  (abs (* (- x2 x1) (- y2 y1))))

;; Tests
(define (inside-unit-circle? pair)
  (< (sqrt (+ (expt (car pair) 2) (expt (cdr pair) 2)))
     1))

(random-seed 0)

(check-within
  (stream-ref (estimate-integral inside-unit-circle? -1 1 -1 1) 10000)
  pi
  (* pi 0.01))
