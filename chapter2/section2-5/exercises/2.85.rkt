#lang racket

(require "2.83.rkt")

(define get void)
(define put void)


(define (drop arg)
  (let ((projected-arg (project arg)))
    (if projected-arg
        (drop projected-arg)
        arg)))

(define (project arg)
  (apply-generic 'project arg))

(define (project-if-valid proc number)
  (let ((try (proc number)))
    (if (equal? (raise try) number)
        try
        false)))


;; Extend packages defined in this section's notes
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (project number)
    (project-if-valid
     (lambda (num)
       ((get 'make 'integer)
        (exact-floor (/ (numer number) (denom number)))))
     number))

  (put 'project 'integer project))

(define (install-real-package)
  (define (project number)
    (project-if-valid
     (lambda (num) ((get 'make 'rational) (exact-floor num) 1))
     number))

  (put 'project 'real project))

(define (install-complex-package)
  (define (project number)
    (project-if-valid
     (lambda (num) ((get 'make 'real) (real-part num)))
     number))

  (put 'project 'complex project))
