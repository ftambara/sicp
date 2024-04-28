#lang racket

(require
  "../../modules/streams.rkt"
  rackunit)

;; a. The integral of the series a0 + a1x + a2x^2 + a3x^3 + ...
;; is the series c + a0x + 1/2 a1x^2 + 1/3 a2x^3 + 1/4 a3x^4 + ... ,
;; where c is any constant. Deﬁne a procedure integrate-series that takes
;; as input a stream a0, a1, a2, ... representing a power series and
;; returns the stream a0, 1/2 a1, 1/3 a2, ... of coefficients of the
;; non-constant terms of the integral of the series. (Since the result has
;; no constant term, it doesn’t represent a power series; when we use
;; integrate-series, we will cons on the appropriate constant.)

(define (invert-stream nums) (stream-map / nums))
(define (integers-from n) (stream-cons n (integers-from (add1 n))))

(check-equal? (stream->list (invert-stream (stream-take (integers-from 1) 5)))
              '(1 1/2 1/3 1/4 1/5))

(define (integrate-series coeff-stream)
  (mult-streams coeff-stream (invert-stream (integers-from 1))))

(check-equal? (stream->list (integrate-series (stream 1 1 1 1)))
              '(1 1/2 1/3 1/4))
(check-equal? (stream->list (integrate-series (stream 2 4 8 16)))
              '(2 2 8/3 4))

;; b. The function x → e^x is its own derivative. This implies that e^x and
;; the integral of e^x are the same series, except for the constant term,
;; which is e^0 = 1. Accordingly, we can generate the series for e x as

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

;; Show how to generate the series for sine and cosine, starting from the facts
;; that the derivative of sine is cosine and the derivative of cosine is the 
;; negative of sine:

(define (stream-opposite stream) (stream-map - stream))
(define cosine-series
  (stream-cons
    1
    (stream-opposite (integrate-series sine-series))))
(define sine-series (stream-cons 0 (integrate-series cosine-series)))

(check-equal? (stream->list (stream-take exp-series 5))
              '(1 1 1/2 1/6 1/24))
(check-equal? (stream->list (stream-take cosine-series 5))
              '(1 0 -1/2 0 1/24))
(check-equal? (stream->list (stream-take sine-series 6))
              '(0 1 0 -1/6 0 1/120))
;; I'm extremely surprised that this works. It's beautiful.
