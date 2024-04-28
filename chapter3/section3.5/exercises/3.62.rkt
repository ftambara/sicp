#lang racket

;; Use the results of Exercise 3.60 and Exercise 3.61 to deﬁne a procedure
;; div-series that divides two power series. div-series should work for any
;; two series, provided that the denominator series begins with a nonzero
;; constant term. (If the denominator has a zero constant term, then
;; div-series should signal an error.)

(require
  "3.59.rkt"
  "3.60.rkt"
  "3.61.rkt"
  "../../modules/streams.rkt"
  rackunit)

;; S1 / S2 = S1 * N1
;; Where N1 = 1 / S2

;; To find the inverse of any series that starts with c != 0:
;; X = 1 / (c + S_R)
;; X = (1/c) (1 / ((c + S_R) / c))
;; X = (1/c) (1 / (1 + S_R / c))

(define (div-series s1 s2)
  (if (= (stream-first s2) 0)
    (error "s2 can't start with 0")
    (let ([c2 (stream-first s2)])
      (mul-series
        s1
        (scale-stream
          (invert-unit-series (scale-stream s2 (/ c2)))
          (/ c2))))))

;; Show how to use div-series together with the result of Exercise 3.59 to
;; generate the power series for tangent.

;; tan x = sin x / cos x
(define tan-series (div-series sine-series cosine-series))
(check-equal? (stream->list (stream-take tan-series 6))
              '(0 1 0 1/3 0 2/15))
