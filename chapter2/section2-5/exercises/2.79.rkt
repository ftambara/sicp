#lang racket

(require "../../section2-4/notes.rkt")


(define put void)
(define get void)

;; On the scheme-number package
(put 'equ? '(scheme-number scheme-number) =)

;; On the rational package
(put 'equ? '(rational rational)
     (lambda (num1 num2) (= (/ (numer num1) (denom num1))
                            (numer num2) (denom num2))))

;; On the complex package
(put 'equ? '(complex complex)
     (lambda (num1 num2) (and (= (magnitude num1) (magnitude num2))
                              (= (angle num1) (angle num2)))))

(define (equ? num1 num2)
  (apply-generic 'equ? num1 num2))
