#lang racket

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;;       ┌───────────────────────────┐
;;       v                           │
;; x-> [ ● | ●-]-->[ ● | ●-]-->[ ● | ● ]
;;       |           |           |
;;       v           v           v
;;       'a          'b          'c

;; What happens if we try to compute (last-pair z)?
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

;; (last-pair z) will be stuck in an infinite recursion loop, since there
;; is no pair whose cdr is null, and all pairs have another pair as a cdr.
