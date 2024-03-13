#lang racket

(define put void)
(define (term-list poly) (cdr poly))
(define (coeff term) (cdr term))
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))

(define (coeffs-zero? terms)
  (if (empty? terms)
    #t
    (and (= (coeff (first-term terms)) 0)
         (coeffs-zero? (rest-terms terms)))))

(put '=zero? 'polynomial
  (lambda (p) (coeffs-zero? (term-list p))))
