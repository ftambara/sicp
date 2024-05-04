#lang racket

(require
  "../../modules/streams.rkt"
  rackunit)

;; The integral procedure used above was analogous to the “implicit” definition
;; of the infinite stream of integers in Section 3.5.2. Alternatively, we can
;; give a definition of integral that is more like integers-starting-from (also
;; in Section 3.5.2):
(define (integral integrand initial-value dt)
  (stream-cons
    initial-value
    (if (stream-null? integrand)
      the-empty-stream
      (integral (stream-cdr integrand)
                (+ (* dt (stream-car integrand))
                   initial-value)
                dt))))
;; When used in systems with loops, this procedure has the same problem as does
;; our original version of integral.
;; Modify the procedure so that it expects the integrand as a delayed argument
;; and hence can be used in the solve procedure shown above.
(define (delayed-integral delayed-integrand initial-value dt)
  (stream-cons
    initial-value
    (let ([integrand (force delayed-integrand)])
      (if (stream-null? integrand)
        empty-stream
        (delayed-integral (delay (stream-cdr integrand))
                          (+ (* dt (stream-car integrand))
                             initial-value)
                          dt)))))

(define (solve f y0 dt)
  (define y (delayed-integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(check-within (stream-ref (solve (lambda (y) y) 1 0.001) 1000)
              (exp 1)
              0.01)
