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

; Exercise 2.2
(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

(define (average a b) (/ (+ a b) 2))

(define (midpoint-segment segment)
    (make-point (average (x-point (start-segment segment))
                         (x-point (end-segment segment)))
                (average (y-point (start-segment segment))
                         (y-point (end-segment segment)))))

(define (make-point x y) (cons x y))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (make-segment start end) (cons start end))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))

; Exercise 2.3

; There are surely better ways to define a rectangle using the
; language of vectors, right-handedness, and/or projections.
; To keep things simple*, I'll define a rectangle using constructors
; that depend on the user making the right calculations beforehand
; (especially about orthogonality), and just check adequacy before
; returning.
; * That was unfortunatelly not true.

(define (length-segment segment)
    (sqrt (+ (square (- (x-point (end-segment segment))
                        (x-point (start-segment segment))))
             (square (- (y-point (end-segment segment))
                        (y-point (start-segment segment)))))))

(define (square x) (* x x))

(define (dot-product vec1 vec2)
    (+ (* (- (x-point (end-segment vec1))
             (x-point (start-segment vec1)))
          (- (x-point (end-segment vec2))
             (x-point (start-segment vec2))))
       (* (- (y-point (end-segment vec1))
             (y-point (start-segment vec1)))
          (- (y-point (end-segment vec2))
             (y-point (start-segment vec2))))))

(define (orthogonal? seg1 seg2)
    (= (dot-product seg1 seg2) 0))

(define (connected? seg1 seg2)
    (or (equal? (start-segment seg1) (start-segment seg2))
        (equal? (start-segment seg1) (end-segment seg2))
        (equal? (end-segment seg1) (start-segment seg2))
        (equal? (end-segment seg1) (end-segment seg2))))

; First rectangle implementation

; (define (make-rectangle base-segment side-segment)
;     (cond ((not (orthogonal? base-segment side-segment))
;             (error "Segments are not orthogonal"))
;           ((not (connected? base-segment side-segment))
;             (error "Segments are not connected"))
;           (else (cons base-segment side-segment))))

; (define (base-rectangle rectangle)
;     (car rectangle))

; (define (side-rectangle rectangle)
;     (cdr rectangle))

; Define in terms of a further abstraction barrier to allow
; the rectangle implementation to change without affecting
; the perimeter and area procedures.

(define (perimeter rectangle)
    (* 2 (+ (length-segment (base-rectangle rectangle))
            (length-segment (side-rectangle rectangle)))))

(define (area rectangle)
    (* (length-segment (base-rectangle rectangle))
       (length-segment (side-rectangle rectangle))))

; Second rectangle implementation

(define (make-rectangle base height)
    (cons base height))

(define (base-rectangle rectangle)
    (car rectangle))

(define (height-rectangle rectangle)
    (cdr rectangle))

(define (rotate-segment segment angle)
    (let* ((end (end-segment segment))
           (x (x-point end))
           (y (y-point end))
           (new-x (+ (* x (cos angle))
                     (* y (sin angle))))
           (new-y (- (* x (sin angle))
                     (* y (cos angle)))))
          (make-segment (start-segment segment)
                        (make-point new-x new-y))))

(define (stretch-segment segment scalar)
    (let ((x-end (x-point (end-segment segment)))
          (y-end (y-point (end-segment segment))))
         (make-segment (start-segment segment)
                       (make-point (* scalar x-end)
                                   (* scalar y-end)))))


(define (side-rectangle rectangle)
    (let ((side-segment (rotate-segment (base-rectangle rectangle) (/ pi 2))))
         (stretch-segment side-segment
                          (/ (height-rectangle rectangle)
                             (length-segment side-segment)))))

; Exercise 2.4

(define (cons-alt x y)
    (lambda (m) (m x y)))

(define (car-alt z)
    (z (lambda (p q) p)))

; (define pair (cons-alt 5 6)) 
; p -> (lambda (m) (m 5 6))
; (car-alt pair)
; ((lambda (m) (m 5 6)) (lambda (p q) p))
; ((lambda (p q) p) 5 6)
; (5)

(define (cdr-alt z)
    (z (lambda (p q) q)))

(cdr-alt (cons-alt 5 6)); => 6

; Exercise 2.5

(define (cons-fact x y)
    (* (expt 2 x) (expt 3 y)))

(define (divisible? a b)
    (= (modulo a b) 0))

(define (car-fact z)
    (define (iter start)
        (cond ((< start 1) (error "Invalid pair."))
              ((divisible? start 3) (iter (/ start 3)))
              (else (exact-round (log start 2)))))
    (iter z))

(define (cdr-fact z)
    (define (iter start)
        (cond ((< start 1) (error "Invalid pair."))
              ((divisible? start 2) (iter (/ start 2)))
              (else (exact-round (log start 3)))))
    (iter z))

(define p (cons-fact 5 3))
(car-fact p); => 5
(cdr-fact p); => 3

; Exercise 2.6

; can manipulate procedures, we can get by without numbers
; (at least insofar as nonnegative integers are concerned) by
; implementing 0 and the operation of adding 1 as
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))
; this representation is known as Church numerals, afer its
; inventor, Alonzo Church, the logician who invented the λ-calculus.
; Define one and two directly (not in terms of zero and add-1).

(add-1 zero)
; zero => (lambda (f) (lambda (x) x))
; add-1 => (lambda (f) (lambda (x) (f ((n f) x))))
; ((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
; ((lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
; ((lambda (f) (lambda (x) (f x))))

(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
; one (lambda (f) (lambda (x) (f x)))
; add-1 => (lambda (f) (lambda (x) (f ((n f) x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

; Remark: Church numerals are extremely concerning.
