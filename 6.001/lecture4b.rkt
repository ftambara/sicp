#lang racket

(define (+c z1 z2)
  (make-rectangular
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
  (make-rectangular
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))

(define (make-rectangular real imag)
  (cons real imag))

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (operate op obj)
    (let ((proc (get op (type obj))))
      (if (null? proc)
        (error "no valid operation", op, (type obj))
        (proc (contents obj)))))

;; This way of putting anonymous lambdas directly in the table
;; (as opposed to first defining an operation and then saving that named
;; procedure) avoids all procedure name clashes (as long as they have
;; different types)
(put 'magnitude 'polar
     (lambda (z) (car z)))

;; builder procedures neet to attach the type tag
(define (make-rat numerator denominator)
  (attach-type 'rational (cons numerator denominator)))

;; operate-n is a generic operation that dispatches on the type of the
;; first argument
(define (operate-n op . args)
  (let ((type (type (car args))))
    ;; Check that all arguments have the same type
    (if (not (all (lambda (x) (eq? type (type x))) args))
        (error "arguments have different types"))
    ;; Get the operation for the type
    (let ((proc (get op type)))
        (if (null? proc)
            (error "no valid operation", op, type)
            ;; Apply the operation to the contents of the arguments
            (apply proc (map contents args))))))

(define (add num1 num2)
  (operate-n 'add num1 num2))

;; This sits one level of abstraction above the rectangular and
;; polar packages
;; This grouping is just for organization purposes. Rectangular and polar
;; types could coexist with other number types on the same level
(define (make-complex z)
  (attach-type 'complex z))

(define (+complex z1 z2)
  (make-complex (+c z1 z2)))

;; By replacing + with add, +c-turbo can now handle coefficents of any other
;; type that supports the add operation
(define (+c-turbo z1 z2)
  (make-rectangular
    (add (real-part z1) (real-part z2))
    (add (imag-part z1) (imag-part z2))))
