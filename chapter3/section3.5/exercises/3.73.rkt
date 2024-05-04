#lang racket

;; We can model electrical circuits using streams to represent the values of
;; currents or voltages at a sequence of times. For instance, suppose we have
;; an RC circuit consisting of a resistor of resistance R and a capacitor of
;; capacitance C in series. The voltage response v of the circuit to an
;; injected current i is determined by the formula in Figure 3.33, whose
;; structure is shown by the accompanying signal-flow diagram.
;; Write a procedure RC that models this circuit. RC should take as inputs the
;; values of R, C, and dt and should return a procedure that takes as inputs a
;; stream representing the current i and an initial value for the capacitor
;; voltage v0 and produces as output the stream of voltages v. For example, you
;; should be able to use RC to model an RC circuit with R = 5 ohms,
;; C = 1 farad, and a 0.5-second time step by evaluating
;; (define RC1 (RC 5 1 0.5)). This deﬁnes RC1 as a procedure that takes a
;; stream representing the time sequence of currents and an initial capacitor
;; voltage and produces the output stream of voltages.

(require
  "../../modules/streams.rkt"
  rackunit)

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (currents-stream v0)
    ;; v = 1/C integral(i, dt) + R*i
    (add-streams
      (scale-stream (integral currents-stream v0 dt) (/ C))
      (scale-stream currents-stream R))))

(define RC1 (RC 5 1 0.5))

(define (constant-current x)
  (define ones
    (stream-cons 1 ones))
  (scale-stream ones x))

(define va (RC1 (constant-current 2) 4))
;; va = 4 V + 2 A * t/C + 5 ohm * 2 A
;; va = 2 A * t/C + 14 V
(check-equal? (map exact->inexact (stream->list (stream-take va 3)))
              '(14.0 15.0 16.0))
