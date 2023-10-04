#lang racket

(require "../../section2-4/notes.rkt")


(define put void)
(define get void)
(define numer void)
(define denom void)
(define magnitude void)
(define angle void)

;; On the scheme-number package
(put '=zero? '(scheme-number) zero?)

;; On the rational package
(put '=zero? '(rational)
     (lambda (num) (and (not (= (denom num) 0))
                        (= (numer num) 0))))

;; On the complex package
(put 'equ? '(complex)
     (lambda (num) (= (magnitude num) 0)))

(define (=zero? num)
  (apply-generic '=zero? num))
