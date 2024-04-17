#lang racket

(require
  "circuits.rkt"
  rackunit)

(define a (make-wire))
(define b (make-wire))
(define out (make-wire))

(and-gate! a b out)
(propagate!)
(reset-time!)

(set-signal! a 1)
(set-signal! b 1)
(set-signal! a 0)
(set-signal! b 0)
(propagate!)

;; FIFO (as implemented) 
;; a is set to 1
;;  -> Schedule an AND action
;;
;; b is set to 1
;;  -> Schedule an AND action
;;
;; a is set to 0
;;  -> Schedule an AND action
;;
;; b is set to 0
;;  -> Schedule an AND action
;;
;; all four and actions are checked with unvarying inputs
;; all wires end up with signal 0, the correct state
(check-eq? (get-signal a) 0)
(check-eq? (get-signal b) 0)
(check-eq? (get-signal out) 0)

;; LIFO
;; b is set to 0
;;  -> Schedule an AND action
;; a && b -> out stays 0
;;
;; a is set to 0
;;  -> Schedule an AND action
;; a && b -> out stays 0
;;
;; b is set to 1
;;  -> Schedule an AND action
;; a && b -> out stays 0
;;
;; a is set to 1
;;  -> Schedule an AND action
;; a && b -> out is set to 1

;; The end state of all wires would be 1; which is not an invalid state,
;; but the behaviour is counterintuitive.
