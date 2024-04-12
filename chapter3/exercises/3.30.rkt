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

;; ripple-carry-delay = n * full-adder-delay
;; full-adder-delay = 2 * half-adder-delay + or-gate-delay
;; half-adder-delay = max(or-gate-delay, and-gate-delay + inverter-delay) 
;;                    + and-gate-delay
