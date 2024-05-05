#lang racket

;; Generalize the solve-2nd procedure of Exercise 3.78 so that it can be used
;; to solve general second-order differential equations
;; d^2y/dt^2 = f(dy/dt, y).

(require "../../modules/streams.rkt")

(define (integral delayed-integrand initial-value dt)
  (define int
    (stream-cons
      initial-value
      (let ((integrand (force delayed-integrand)))
        (add-streams (scale-stream integrand dt) int))))
  int)

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (f dy y))
  y)

;; Tests
;; Say f(y', y) = ay' + by
(define (f a b)
  (lambda (dy y)
    (add-streams (scale-stream dy a) (scale-stream y b))))
(stream-display (stream-take (solve-2nd (f -1.0 2.0) 0.01 5 3) 10))
;; 5
;; 5.03
;; 5.060700000000001
;; 5.092099000000001
;; 5.124196150000001
;; 5.156990748300001
;; 5.190482239847001
;; 5.224670214628191
;; 5.2595544061095385
;; 5.295134689718998
