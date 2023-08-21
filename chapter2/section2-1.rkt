#lang racket

; (define (make-rat numerator denominator)
;     ...)

; (define (numerator rational-number)
;     ...)

; (define (denominator rational-number)
;     ...)

; Define the rules we want in terms of procedures
; we wish we had. Then implement those.

(define (add-rat x y)
    (make-rat (+ (* (numerator x) (denominator y))
                 (* (numerator y) (denominator x)))
              (* (denominator x) (denominator y))))

(define (sub-rat x y)
    (make-rat (- (* (numerator x) (denominator y))
                 (* (numerator y) (denominator x)))
              (* (denominator x) (denominator y))))

(define (mul-rat x y)
    (make-rat (* (numerator x) (numerator y))
              (* (denominator x) (denominator y))))

(define (div-rat x y)
    (make-rat (* (numerator x) (denominator y))
              (* (denominator x) (numerator y))))

(define (equal-rat? x y)
    (= (* (numerator x) (denominator y)) 
       (* (denominator x) (numerator y))))

(define (make-rat numerator denominator) 
    (let ((divisor (echo (gcd numerator denominator))))
         (cons (/ numerator divisor) 
               (/ denominator divisor))))

(define (numerator rational-number) (car rational-number))

(define (denominator rational-number) (cdr rational-number))

(define (print-rat rational-number)
    (display (numerator rational-number))
    (display "/")
    (display (denominator rational-number)))

(define (echo x)
    (display x)
    (newline)
    x)

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (modulo a b))))

; Exercise 2.1
; The above algorith for gcd seems to do a good job
; at returning positive if a is positive, negative if
; b is negative. This already does the normalization we
; want. Just in case, here is a sign-checked version:
(define (norm-make-rat num den)
    (let* ((divisor (gcd num den))
           (norm-div 
                (if (and (positive? num) (negative? (* num den))) 
                    (- (abs divisor))
                    (abs divisor))))
         (cons (/ num norm-div) (/ den norm-div))))