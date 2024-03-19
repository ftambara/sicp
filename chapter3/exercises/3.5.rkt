#lang racket

(require rackunit)

(define (inside-unit-circle? x y)
  (< (sqrt (+ (expt x 2) (expt y 2)))
     1))
(check-true (inside-unit-circle? 0.5 0.5))
(check-true (inside-unit-circle? 0.9 0))
(check-false (inside-unit-circle? 1.1 0))

(define (rand-range a b)
  (+ a (* (random) (- b a))))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (monte-carlo
    trials
    (lambda ()
      (pred (rand-range x1 x2) (rand-range y1 y2)))))

(define (monte-carlo trials pred)
  (define (iter passed remaining)
    (cond ((= remaining 0)
           (/ passed trials))
          ((pred)
           (iter (+ passed 1) (- remaining 1)))
          (else
           (iter passed (- remaining 1)))))
  (iter 0 trials))

(random-seed 0)
(check-within
  (* 4 (estimate-integral inside-unit-circle? -1 1 -1 1 10000))
  pi
  (* pi 0.01))
