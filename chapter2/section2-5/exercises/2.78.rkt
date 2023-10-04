#lang racket

(require rackunit)


(define (literal? expr)
  (or (number? expr) (symbol? expr)))

(define (attach-tag type-tag contents)
  (if (literal? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        ((symbol? datum) 'scheme-symbol)
        (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((literal? datum) datum)
        (error "Bad tagged datum: CONTENTS" datum)))

;; Tests
(check-equal? (type-tag 10) 'scheme-number)
(check-equal? (type-tag (list 'complex (cons 4 5)))
              'complex)

(check-equal? (contents 10) 10)
(check-equal? (contents (list 'complex (cons 4 5)))
              (list (cons 4 5)))
