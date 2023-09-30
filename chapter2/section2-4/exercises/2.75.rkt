#lang racket

(require rackunit)


(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define complex-num (make-from-mag-ang 4 (/ pi 3)))

(check-within (complex-num 'real-part) 2 0.001)
(check-equal? (complex-num 'imag-part) (* 4 (sqrt 3) (/ 2.0)))
(check-equal? (complex-num 'magnitude) 4)
(check-equal? (complex-num 'angle) (/ pi 3))
