#lang racket

;; A series RLC circuit consists of a resistor, a capacitor, and an inductor
;; connected in series, as shown in Figure 3.36. If R, L, and C are the
;; resistance, inductance, and capacitance, then the relations between voltage
;; (v) and current (i) for the three components are described by the equations
;;
;;  vR = iR * R,    vL = L diL/dt,    iC = C dvC/dt
;;
;; and the circuit connections dictate the relations
;;
;;  iR = iL = -iC,    vC = vR + vL
;;
;; Combining these equations shows that the state of the circuit (summarized by
;; vC, the voltage across the capacitor, and iL, the current in the inductor)
;; is described by the pair of differential equations
;;
;; dvC/dt = -iL/C,    diL/dt = 1/L * vC - R/L * iR
;;
;; The signal-flow diagram representing this system of differential equations
;; is shown in Figure 3.37.
;;
;; Write a procedure RLC that takes as arguments the parameters R, L, and C of
;; the circuit and the time increment dt. In a manner similar to that of the RC
;; procedure of Exercise 3.73, RLC should produce a procedure that takes the
;; initial values of the state variables, vC0 and iL0, and produces a pair
;; (using cons) of the streams of states vC and iL. Using RLC, generate the
;; pair of streams that models the behavior of a series RLC circuit with
;; R = 1 ohm, C = 0.2 farad, L = 1 henry, dt = 0.1 second, and initial values
;; iL0 = 0 amps and vC0 = 10 volts.

(require "../../modules/streams.rkt")

(define (integral delayed-integrand initial-value dt)
  (define int
    (stream-cons
      initial-value
      (let ((integrand (force delayed-integrand)))
        (add-streams (scale-stream integrand dt) int))))
  int)

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL
      (integral
        (delay (add-streams (scale-stream vC (/ L))
                            (scale-stream iL (- (/ R L)))))
        iL0
        dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (stream-map-all cons vC iL)))

;; Tests
(define circuit (RLC 1 1 0.2 0.1))
(stream-display (stream-take (circuit 10 0) 10))
;; (10 . 0)
;; (10 . 1.0)
;; (9.5 . 1.9)
;; (8.55 . 2.66)
;; (7.220000000000001 . 3.249)
;; (5.5955 . 3.6461)
;; (3.77245 . 3.84104)
;; (1.8519299999999999 . 3.834181)
;; (-0.0651605000000004 . 3.6359559)
;; (-1.8831384500000004 . 3.2658442599999997)
