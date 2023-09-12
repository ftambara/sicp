#lang racket

(require rackunit)


(define (equal-rec? elem1 elem2)
  (if (and (pair? elem1) (pair? elem2))
      (and (equal-rec? (car elem1) (car elem2))
           (equal-rec? (cdr elem1) (cdr elem2)))
      (eq? elem1 elem2)))

; Solved some type bugs thanks to
; https://github.com/sarabander/p2pu-sicp/blob/master/2.3/2.54.scm

(check-true (equal-rec? '(this is a list) '(this is a list)))
(check-false (equal-rec? '(this is a list) '(this (is a) list)))
