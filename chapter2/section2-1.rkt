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

; ====================================================================
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

; ====================================================================
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

; ====================================================================
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

; ====================================================================
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

; ====================================================================
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

; ====================================================================
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

; (add-1 zero)
; zero => (lambda (f) (lambda (x) x))
; add-1 => (lambda (f) (lambda (x) (f ((n f) x))))
; ((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
; ((lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
; ((lambda (f) (lambda (x) (f x))))

(define one (lambda (f) (lambda (x) (f x))))

; (add-1 one)
; one (lambda (f) (lambda (x) (f x)))
; add-1 => (lambda (f) (lambda (x) (f ((n f) x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

; Remark: Church numerals are extremely concerning.

(define (sum-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
         (make-interval (min p1 p2 p3 p4)
                        (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y)))))

; ====================================================================
; Exercise 2.7

(define (make-interval a b)
    (cons a b))

(define (upper-bound interval)
    (max (car interval) (cdr interval)))

(define (lower-bound interval)
    (min (car interval) (cdr interval)))

; ====================================================================
; Exercise 2.8

; When subtracting, the lower and upper bound of the subtrahend
; get flipped due to the sign change
(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

; ====================================================================
; Exercise 2.9

(define (width-interval interval)
    (/ (- (upper-bound interval) (lower-bound interval)) 2))

; Using applicative order substitution:

; (width-interval (add-interval (make-interval a b) (make-interval c d)))
; ...
; (width-interval (make-interval (+ a c) (+ b d)))
; (/ (- (+ b d) (+ a c)) 2)

; This is equivalent to

; (/ (+ (- b a) (- d c)) 2)
; (+ (/ (- b a) 2) (/ (- d c) 2))
; (+ (width-interval (make-interval a b)) (width-interval (make-interval c d)))

; This shows that the width of two added intervals is equivalent
; to the sum of the widths of each interval. Therefore, the result
; is dependent only the widths of the intervals.

; In the case of multiplication, the following cases operate with
; intervals of identical widths, yet the results are different:

(width-interval (make-interval 5 9)); => 2
(width-interval (mul-interval (make-interval 1 3)
                              (make-interval 5 9))); => 11

(width-interval (make-interval 2 6)); => 2
(width-interval (mul-interval (make-interval 1 3)
                              (make-interval 2 6))); => 8

; These examples prove that the width of the product of two intervals
; is not uniquely determined by the width of the factors.

; ====================================================================
; Exercise 2.10

(define (spans? interval number)
    (and (>= number (lower-bound interval))
         (<= number (upper-bound interval))))

(define (div-interval-safe x y)
    (if (spans? y 0)
        (error "Interval" y "includes 0.")
        (div-interval x y)))

; ====================================================================
; Exercise 2.11

; Table of all possible sign combinations:
; (+ +) (+ +)
; (+ +) (+ -)   NOT VALID
; (+ +) (- +)
; (+ +) (- -)
; (+ -) (+ +)   NOT VALID
; (+ -) (+ -)   NOT VALID
; (+ -) (- +)   NOT VALID
; (+ -) (- -)   NOT VALID
; (- +) (+ +)
; (- +) (+ -)   NOT VALID
; (- +) (- +)
; (- +) (- -)
; (- -) (+ +)
; (- -) (+ -)  NOT VALID
; (- -) (- +)
; (- -) (- -)

; Summary of valid cases
; 1 (+ +) (+ +)   
; 2 (+ +) (- +)   
; 3 (+ +) (- -)   
; 4 (- +) (+ +)   
; 5 (- +) (- +)   
; 6 (- +) (- -)   
; 7 (- -) (+ +)   
; 8 (- -) (- +)   
; 9 (- -) (- -)   
; Nine cases in total, as expected

(define (positive? num)
    (> num 0))

(define (negative? num)
    (< num 0))

(define (mul-interval-ben x y)
    (define n? negative?)
    (define p? positive?)

    (let ((lower-x (lower-bound x))
          (upper-x (upper-bound x))
          (lower-y (lower-bound y))
          (upper-y (upper-bound y)))
        (cond 
            ; Zeroes treated as negative numbers. As far as max and min
            ; logic goes, they don't make a difference.
            ; 1 (+ +) (+ +)
            ((and (p? lower-x) (p? lower-y))
             (make-interval (* lower-x lower-y) (* upper-x upper-y)))

            ; 2 (+ +) (-/0 +)
            ((and (p? lower-x) (p? upper-y))
             (make-interval (* upper-x lower-y) (* upper-x upper-y)))

            ; 3 (+ +) (-/0 -/0)
            ((p? lower-x)
             (make-interval (* upper-x lower-y) (* lower-x lower-y)))

            ; 4 (-/0 +) (+ +)
            ((and (p? upper-x) (p? lower-y))
             (make-interval (* lower-x upper-y) (* upper-x upper-y)))

            ; 5 (-/0 +) (-/0 +)
            ((and (p? upper-x) (p? upper-y))
             (make-interval (min (* lower-x upper-y) (* upper-x lower-y))
                            (max (* lower-x lower-y) (* upper-x upper-y))))

            ; 6 (-/0 +) (-/0 -/0)
            ((p? upper-x)
             (make-interval (* upper-x lower-y) (* lower-x lower-y)))

            ; 7 (-/0 -/0) (+ +)
            ((p? lower-y)
             (make-interval (* lower-x upper-y) (* upper-x lower-y)))

            ; 8 (-/0 -/0) (-/0 +)
            ((p? upper-y)
             (make-interval (* lower-x upper-y) (* lower-x lower-y)))

            ; 9 (-/0 -/0) (-/0 -/0)
            (else
             (make-interval (* upper-x upper-y) (* lower-x lower-y))))))

(define i1 (make-interval 4 5))
(define i2 (make-interval 1 2))
(mul-interval-ben i1 i2); => '(4 . 10)

(define i3 (make-interval -4 5))
(define i4 (make-interval -1 2))
(mul-interval-ben i3 i4); => '(-8 . 10)

(define i5 (make-interval -4 5))
(define i6 (make-interval -3 2))
(mul-interval-ben i5 i6); => '(-15 . 12)

; ====================================================================
; Exercise 2.12

(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
    (make-center-width c (* c (/ p 100))))

(define (percent interval)
    (/ (* (/ (- (upper-bound interval) (lower-bound interval)) 2)
          100)
       (center interval)))

; ====================================================================
; Exercise 2.13

; To get the width and center of the product,
; we start by calculating its extremes.
; (Assuming extremes of factors are themselves positive)
;   low-bound = (ca - wa) (cb - wb)
;       cx: center of interval x
;       wx: width of interval x

;   low-bound = (ca - ca * pa / 100) (cb - cb * pb / 100)
;       px: percentage of interval x
;   low-bound = ca cb (1 - pa / 100) (1 - pb / 100)
;   low-bound = ca cb (1 - pa / 100 - pb / 100 + pa * pb / 100^2)

; If both pa and pb are small, their product will be twice as small.
; Therefore, by approximation
;   low-bound = ca cb (1 - pa / 100 - pb / 100)

; Similarly, we get
;   upp-bound = ca cb (1 + pa / 100 + pb / 100)

; With these bounds, we can calculate the width
;   width = (upp-bound - low-bound) / 2
;   width = ca cb ((pa / 100) + (pb / 100))

; ...and the center of the product
;   center = (upp-bound + low-bound) / 2
;   center = ca cb

; The tolerance percentage, then, can be expressed as
; percentage = (width / center) * 100
; percentage = pa + pb

(define (tolerance interval)
    (/ (percent interval) 100))

(define (mul-interval-approx x y)
    (define (small-enough? a b)
        (< a (/ b 10)))
    (cond ((not (small-enough?
                (* (tolerance x) (tolerance y))
                (/ (+ (tolerance x) (tolerance y)) 2)))
            (display "\nTolerances too large, using backup function\n")
            (mul-interval x y))
          ((or (negative? (lower-bound x)) (negative? (lower-bound y)))
            (display "\nSome intervals contain negative numbers, ")
            (display "using backup function\n")
            (mul-interval x y))
          (else (make-center-percent (* (center x) (center y))
                                     (+ (percent x) (percent y))))))
            

(define i7 (make-center-percent 4 1))
(define i8 (make-center-percent 10 2))
(mul-interval-approx i7 i8); => '(38.8, 41.2)
(mul-interval i7 i8); => '(38.808, 41.208)

; ====================================================================
; Exercise 2.14

(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (sum-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval
            one
            (sum-interval (div-interval one r1)
                          (div-interval one r2)))))

(define (print-cp interval)
    ; Display floats with 3 decimal places
    (display "(c: ")
    (display (~r #:precision '(= 3) (center interval)))
    (display ", p: ")
    (display (~r #:precision '(= 3) (percent interval)))
    (display ")\n"))

(display "\nExercise 2.13\n")
(print-cp i1); (c: 4.500, p: 11.111)
(print-cp i2); (c: 1.500, p: 33.333)
(define int-par1 (par1 i1 i2))
(define int-par2 (par2 i1 i2))
(print-cp int-par1); (c: 1.286, p: 55.556)
(print-cp int-par2); (c: 1.114, p: 28.205)
; Lem is right, the results are indeed different.

; Some more tests
(define one-int (make-interval 1 1))
(print-cp i7); (c: 4.000, p: 1.000)
(print-cp i8); (c: 10.000, p: 2.000)
(print-cp (div-interval i7 i7)); (c: 1.000, p: 2.000)
(print-cp (div-interval i8 i8)); (c: 1.001, p: 3.998)
(print-cp (div-interval one-int i7)); (c: 0.250, p: 1.000)
(print-cp (div-interval i7 i8)); (c: 0.400, p: 2.999)
(print-cp (div-interval i8 i7)); (c: 2.501, p: 2.999)
(define two-int (make-interval 2 2))
(print-cp (sum-interval (div-interval i7 two-int)
                        (div-interval i7 two-int))); (c: 4.000, p: 1.000)
; It seems that division adds tolerances much like
; multiplication does, at least for small tolerance values.
