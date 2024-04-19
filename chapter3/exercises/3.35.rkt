#lang racket

(require
  "constraints.rkt"
  rackunit)

(define (squarer! a b)
  (define (process-new-value)
    (cond ((has-value? a)
           (set-value! b (* (get-value a) (get-value a)) me))
          ((has-value? b)
           (if (< (get-value b) 0)
             (error ("square less than 0"))
           (set-value! a (sqrt (get-value b)) me)))
          (else 'do-nothing)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect! a me)
  (connect! b me))

(define a (make-connector))
(define b (make-connector))
(squarer! a b)

(check-false (has-value? a))
(check-false (has-value? b))

(set-value! a 4 'me)
(check-eq? (get-value b) 16)

(forget-value! a 'me)
(set-value! b 100 'me)
(check-eq? (get-value a) 10)

(forget-value! b 'me)
(check-exn exn:fail? (lambda () (set-value! b -10)))
