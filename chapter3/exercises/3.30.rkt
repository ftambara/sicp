#lang racket

(define make-wire void)
(define full-adder! void)

(define (ripple-carry-adder! c a-list b-list s-list)
  (define (ripple-carry-adder-proc! c-in a-list b-list s-list)
    (if (empty? a-list)
      'ok
      (let ((c-out (make-wire)))
        (full-adder! c-in
                     (car a-list)
                     (car b-list)
                     (car s-list)
                     c-out)
        (ripple-carry-adder-proc! c-out
                                  (cdr a-list)
                                  (cdr b-list)
                                  (cdr s-list)))))

  (if (not (= (length a-list) (length a-list) (length a-list)))
    (error "Disparate wire list lenghts")
    (ripple-carry-adder-proc! c a-list b-list s-list)))

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
