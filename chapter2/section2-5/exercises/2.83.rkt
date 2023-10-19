#lang racket

(provide
 apply-generic
 type-tag
 contents
 raise)

(define get void)
(define put void)

;; From the book
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

;; Extend packages defined in this section's notes
(define (install-integer-package)
  (define (raise number)
    ((get 'make 'rational) number 1))

  (put 'raise 'integer raise))

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (raise number)
    ((get 'make 'real) (/ (numer number) (denom number))))

  (put 'raise 'integer raise))

(define (install-real-package)
  (define (raise number)
    ((get 'make-from-real-imag 'rectangular) number 0))

  (put 'raise 'real raise))

(define (raise number)
  (apply-generic 'raise number))
