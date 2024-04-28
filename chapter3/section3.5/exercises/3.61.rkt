#lang racket

(require
  "3.59.rkt"
  "3.60.rkt"
  rackunit)

;; Let S be a power series (Exercise 3.59) whose constant term is 1.
;; Suppose we want to ﬁnd the power series 1/S, that is, the series X such
;; that S X = 1. Write S = 1 + S_R where S_R is the part of S after the
;; constant term. Then we can solve for X as follows:
;; S · X = 1,
;; (1 + S_R ) · X = 1,
;; X + S_R · X = 1,
;; X = 1 − S_R · X      <- The key is that S_R*X does not contain a constant
;;                          term, so we know X starts with 1
;;
;; In other words, X is the power series whose constant term is 1 and whose
;; higher-order terms are given by the negative of S_R times X. Use this idea
;; to write a procedure invert-unit-series that computes 1/S for a power
;; series S with constant term 1. You will need to use mul-series from
;; Exercise 3.60.

(define (stream-opposite stream) (stream-map - stream))

(define (invert-unit-series s)
  (if (not (= (stream-first s) 1))
    (error "Stream's must start with 1")
    (stream-cons
      1
      (stream-opposite (mul-series (stream-rest s)
                                    (invert-unit-series s))))))

(check-equal?
  (stream->list
    (stream-take
      (mul-series exp-series (invert-unit-series exp-series))
      5))
  '(1 0 0 0 0))
