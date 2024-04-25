#lang racket

(require
  "../../modules/circuits.rkt"
  rackunit)

(define (or-gate! in1 in2 out)
  (let ((a (make-wire))
        (b (make-wire))
        (c (make-wire)))
  (inverter! in1 a)
  (inverter! in2 b)
  (and-gate! a b c)
  (inverter! c out)
  'ok))

;; Tests
(define in1 (make-wire))
(define in2 (make-wire))
(define out (make-wire))

(or-gate! in1 in2 out)

(check-equal? (get-signal out) 0)
(set-signal! in1 1)
(propagate!)
(check-equal? (get-signal out) 1)
