#lang racket

(define make-wire void)
(define inverter! void)
(define and-gate! void)

(define (or-gate! in1 in2 out)
  (let ((a (make-wire))
        (b (make-wire))
        (c (make-wire)))
  (inverter! in1 a)
  (inverter! in2 b)
  (and-gate! a b c)
  (inverter! c out)))
