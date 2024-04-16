#lang racket

(require
  "circuits.rkt"
  rackunit)

(define (ripple-carry-adder! a-list b-list s-list c)
  (define (ripple-carry-adder-proc! c-in a-list b-list s-list c-out)
    (if (empty? a-list)
      'ok
      (begin
        (full-adder! (car a-list)
                     (car b-list)
                     c-in
                     (car s-list)
                     c-out)
        (ripple-carry-adder-proc! (make-wire)
                                  (cdr a-list)
                                  (cdr b-list)
                                  (cdr s-list)
                                  c-in))))

  (if (not (= (length a-list) (length a-list) (length a-list)))
    (error "Disparate wire list lenghts")
    (ripple-carry-adder-proc! (make-wire) a-list b-list s-list c)))

;; What is the delay needed to obtain the complete output from an n-bit
;; ripple-carry adder, expressed in terms of the delays for and-gates,
;; or-gates, and inverters?

;; Assuming n * fa-cout-delay > fa-s-delay
;; rc-delay = n * fa-cout-delay
;;
;; fa-cout-delay = max(ha-c-delay, ha-s-delay + ha-c-delay)
;;                     + or-gate-delay
;;
;; since ha-a-s-delay >= 0
;; fa-cout-delay = ha-s-delay + ha-c-delay + or-gate-delay
;;
;; ha-s-delay = max(or-gate-delay, and-gate-delay + inverter-delay)
;;              + and-gate-delay
;; ha-c-delay = and-gate-delay
;;
;; if or-gate-delay > and-gate-delay + inverter-delay:
;; fa-cout-delay = (or-gate-delay + and-gate-delay) + and-gate-delay + or-gate-delay
;; fa-cout-delay = 2 * (or-gate-delay + and-gate-delay)
;;
;; else:
;; fa-cout-delay = (and-gate-delay + inverter-delay + and-gate-delay)
;;                 + and-gate-delay
;;                 + or-gate-delay
;; fa-cout-delay = 3 * and-gate-delay + inverter-delay + or-gate-delay

;; Tests
(define (wires->bits wire-list)
  (define (loop wire-list)
    (if (null? wire-list)
      null
      (cons (get-signal (car wire-list))
            (loop (cdr wire-list)))))
  (loop wire-list))

(define a-list (list (make-wire) (make-wire) (make-wire)))
(define b-list (list (make-wire) (make-wire) (make-wire)))
(define c (make-wire))
(define s-list (list (make-wire) (make-wire) (make-wire)))

(check-equal? (wires->bits a-list) '(0 0 0))
(ripple-carry-adder! a-list b-list s-list c)

(set-signal! (caddr a-list) 1)
(propagate!)
(check-equal? (wires->bits a-list) '(0 0 1))
(check-eq? (get-signal c) 0)
(check-equal? (wires->bits b-list) '(0 0 0))
(check-equal? (wires->bits s-list) '(0 0 1))

(set-signal! (caddr b-list) 1)
(propagate!)
(check-equal? (wires->bits a-list) '(0 0 1))
(check-equal? (wires->bits b-list) '(0 0 1))
(check-eq? (get-signal c) 0)
(check-equal? (wires->bits s-list) '(0 1 0))

(set-signal! (car a-list) 1)
(set-signal! (cadr b-list) 1)
(propagate!)
(check-equal? (wires->bits a-list) '(1 0 1))
(check-equal? (wires->bits b-list) '(0 1 1))
(check-eq? (get-signal c) 1)
(check-equal? (wires->bits s-list) '(0 0 0))

(set-signal! (car b-list) 1)
(propagate!)
(check-equal? (wires->bits a-list) '(1 0 1))
(check-equal? (wires->bits b-list) '(1 1 1))
(check-eq? (get-signal c) 1)
(check-equal? (wires->bits s-list) '(1 0 0))
