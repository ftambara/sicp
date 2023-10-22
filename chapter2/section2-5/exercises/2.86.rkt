#lang racket

(require "2.83.rkt")


(define (raise-to-real num)
  (if num
      (if (equal? (type-tag num) 'real)
          num
          (raise-to-real (raise num)))
      (error "Cannot raise to real" num)))

(define (exponentiate base exponent)
  (define b (contents (raise-to-real base)))
  (define e (contents (raise-to-real exponent)))
  (expt b e))

(define (cosine angle)
  (define a (contents (raise-to-real angle)))
  (cos a))

(define (sine angle)
  (define a (contents (raise-to-real angle)))
  (sin a))

(define (arctan numer denom)
  (define n (contents (raise-to-real numer)))
  (define d (contents (raise-to-real denom)))
  (atan n d))

(define (multiply mul1 mul2)
  (define m1 (contents (raise-to-real mul1)))
  (define m2 (contents (raise-to-real mul2)))
  (* m1 m2))

(define (install-rectangular-package)
  ;; ...Same as before

  (define (magnitude z)
    (sqrt (+ (exponentiate (real-part z) 2)
             (exponentiate (imag-part z) 2))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a)
    (cons (multiply r (cosine a)) (multiply r (sine a))))

  ;; ...
  'done)

(define (install-polar-package)
  ;; ...Same as before

  (define (real-part z) (multiply (magnitude z) (cosine (angle z))))
  (define (imag-part z) (multiply (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (exponentiate x 2) (exponentiate y 2)))
          (arctan y x)))

  ;; ...
  'done)
