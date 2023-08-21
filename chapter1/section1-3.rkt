#lang racket


(display "\n===[ START ]===\n")

(define (print x)
    (display x)
    (newline)
    x
)

(define (increment n) (+ n 1))
(define (square n) (* n n))

(define (sum term a b next)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) b next))
    )
)

(define (pi-sum a b)
    (define (pi-term num)
        (/ 1.0 (* num (+ num 2))))
    (define (pi-next num)
        (+ num 4))
    (sum pi-term a b pi-next)
)

(define (integral f a b dx)
    (define (next-x x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) b next-x)
       dx)
)

(define (integral-simpson f a b n)
    (define h (/ (- b a) n))
    (define (simpson-term k)
        (f (+ a (* k h)))
    )
    (define (simpson-next k) (+ k 2))

    (* (/ h 3.0)
       (+ (simpson-term 0)
          (simpson-term n)
          (* 4.0 (sum simpson-term 1 (- n 1) simpson-next))
          (* 2.0 (sum simpson-term 2 (- n 2) simpson-next))
        )
    )
)

(define (sum-iter term a b next)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a))))
    )
    (iter a 0)
)

(define (accumulate a b accum next start)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (accum a result))
        )
    )
    (iter (next a) start)
)

(define (ident x) x)

(define (product term a b next) 
    (accumulate a b (lambda (x total) (* (term x) total)) next (term a))
)
(define (factorial n)
    (cond
        ((< n 0) (display "Error: n cannot be less than 0"))
        ((= n 0) 1)
        (else (product ident 1 n increment)))
)

(define (approx-pi n) 
    (if (and (= (modulo n 2) 0) (>= n 4))
        (* 4.0 (/ (print (* 2.0 (+ 2 n) (product square 4 n (lambda (x) (+ x 2)))))
                  (print (product square 3 (+ n 1) (lambda (x) (+ x 2))))))
        (display "Error: n must be even and greater than 4")
    )
)

(define (product-rec term a b next)
    (if (> a b)
        1
        (* (term a) (product-rec term (next a) b next))
    )
)

(define (accumulate-b combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))
        )
    )
    (iter a null-value)
)

(define (filtered-accumulate combiner null-value term a next b filter)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (if (filter (term a)) (combiner (term a) result) result))
        )
    )
    (iter a null-value)
)

(define (even? num)
    (= (modulo num 2) 0)
)

(define (f1.33a a b)
    ; Sum of squares of the even numbers
    ; in the interval a to b
    (filtered-accumulate + 0 square a increment b even?)
)

(define (gcd a b)
    (define (gcd-iter num)
        (cond 
            ((< num 1) (display "Error: gcd must take numbers >= 1"))
            ((and (= (modulo a num) 0) (= (modulo b num) 0)) num)
            (else (gcd-iter (- num 1)))
        )
    )
    (gcd-iter (min a b))
)


(define (f1.33b n)
    (define (relatively-prime? x)
        (= (gcd x n) 1)
    )
    ; Product of all positive integers 
    ; smaller than n
    (filtered-accumulate * 1 ident 1 increment n relatively-prime?)
)

(define (f x y)
    (define (f-helper a b)
        (+ (* x (square a))
           (* y b)
           (* a b)))
    (f-helper (+ 1 (* x y))
              (- 1 y)))

(define (f2 x y)
    ((lambda (a b)
        (+ (* (square a))
            (* y b)
            (* a b)
        )) (+ 1 (* x y)
            (- 1 y))))

(define (f3 x y)
    (let
        ((a (+ 1 (* x y)))
         (b (- 1 y)))
        (+ (* (square a))
            (* y b)
            (* a b))
    )
)

; Exercise 1.34

; (define (f g) (g 2))

; If we call (f f), it will evaluate to (f 2), 
; which is (2 2). 2 is not applicable,
; so the combination will fail.

(define (average a b) (/ (+ a b) 2.0))

(define (echo arg)
    (display "Echo\n")
    arg
)

(define (search f neg_point pos_point tol)
    (define (close_enough? a b tol) 
        (< (abs (/ (- a b) (average a b))) tol))

    (let ((midpoint (average neg_point pos_point)))
        (echo (if (close_enough? neg_point pos_point tol)
            midpoint
            (let ((test_value (f midpoint)))
                (cond 
                    ((positive? test_value)
                        (search f neg_point midpoint tol))
                    ((negative? test_value)
                        (search f midpoint pos_point tol))
                    (else midpoint)))))))

(define (half_interval_method f a b)
    (let ((a_value (f a))
          (b_value (f b)))
        (cond ((and (negative? a_value) (positive? b_value))
                (search f a b 0.01))
              ((and (positive? a_value) (negative? b_value))
                (search f b a 0.01))
              (else (error "Values are not of opposite sign" a b))))
)

(define tolerance 0.0001)

(define (fixed_point f first_guess)
    (define (close_enough? a b)
        (< (abs (- a b)) tolerance))
    
    (define (try guess)
        (display guess)
        (newline)
        (let ((next (f guess)))
            (if (close_enough? guess next)
                next
                (try next))))
    
    (try first_guess))

(define (sqrt x)
    (fixed_point (lambda (y) (average y (/ x y))) 1.0))

(define golden_ratio
    (fixed_point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define 1.36.1 (fixed_point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define 1.36.2 (fixed_point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))

(define (cont_frac_rec n d k)
    (define (rec i)
        (if (= i k)
            (/ (n i) (d i))
            (/ (n i) (+ (d i) (rec (+ i 1))))))
    (rec 1))

; Iterative approach to continuous fraction implementation
(define (cont_frac_iter n d k)
    (define (iter i total)
        ; (display total)
        ; (newline)
        (let ((new_total (/ (n i) (+ (d i) total))))
            (if (= i 1)
                new_total
                (iter (- i 1) new_total))))
    (iter k (/ (n k) (d k))))

(define (divisible? numerator denominator)
    (= (modulo numerator denominator) 0))

(define e_approx
    (let ((d (lambda (i)
        (if (divisible? (+ i 1) 3)
            (* 2.0 (/ (+ i 1) 3))
            1.0))))
    (+ (cont_frac_iter (lambda (x) 1.0) d 10) 2)))

(display "ln e ~= ")
(display (log e_approx))

(define (tan_cf x k)
    (let 
        ((n (lambda (i)
            (if (= i 1)
                x
                (- (square x)))))
         (d (lambda (i) (- (* 2.0 i) 1))))
        (cont_frac_rec n d k)))
    
(define (average_damp f)
    (lambda (x) (average x (f x))))

(define (sqrt_damp x)
    (fixed_point (average_damp (lambda (y) (/ x y)))
                 1.0))

(define (derivative g)
    (let ((dx 0.000001))
        (lambda (x)
            (/ (- (g (+ x dx)) (g x)) dx))))

(define (newton_transform g)
    (lambda (x) (- x (/ (g x) ((derivative g) x)))))

(define (newtons_method g guess)
    (fixed_point (newton_transform g) guess))

(define (sqrt_newton x)
    (newtons_method (lambda (y) (- (square y) x)) 1.0))

; To express the extent to that both sqrt methods are the same idea,
; we can extract that similarity into an abstract procedure
(define (fixed_point_of_transform g transform guess)
    (fixed_point (transform g) guess))

(define (transformed_sqrt_damp x)
    (fixed_point_of_transform 
        (lambda (y) (/ x y))
        average_damp
        1.0))

(define (transformed_sqrt_newton x)
    (fixed_point_of_transform
        (lambda (y) (- (square y) x))
        newton_transform
        1.0))

; Abstracting a procedure
; commented out due to missing definitions from
; Berkeley course (emtpy?, bf, se)
; (define (filter_list seq filter_fn)
;     (cond ((empty? seq) '())
;           ((filter_fn (first seq))
;             (se (first seq) (filter_list (bf seq) filter_fn)))
;           (else (filter_list (bf seq) filter_fn))))

; (define (roots a b c)
;    (lambda (discriminant)
;        (se (/ (+ (- b) discriminant) (* 2 a))
;            (/ (- (- b) discriminant) (* 2 a)) ))
;    ((sqrt (- (* b b) (* 4 a c)))))

; Exercise 1.40
(define (cubic a b c)
    (lambda (x) (+ (* a x x x) (* b x x) (* c x))))

; Exercise 1.41
(define (double fn)
    (lambda (x) (fn (fn x))))

; Exercise 1.42
(define (compose f g)
    (lambda (x) (f (g x))))

; Exercise 1.43
(define (repeated f n)
    (cond ((< n 1) (error "n cannot be less than one"))
          ((= n 1) f)
          (else (compose f (repeated f (- n 1))))))

; Exercise 1.44
(define dx 0.00001)
(define (smooth f)
    (lambda (x) (/ (+ (f (- x dx))
                      (f x)
                      (f (+ x dx)))
                   3)))

; n-fold smoothed function
; ((repeated smooth n) function)

; Exercise 1.45
(define (nth_root x n damps)
    (fixed_point_of_transform 
        (lambda (y) (/ x (expt y (- n 1))))
        (repeated average_damp damps)
        1.0))

; Experimentally, I found that the number of
; damps needed correspond with (floor (log n 8))

; Exercise 1.46
(define (iterative_improve good_enough? improve)
    (define (iter guess)
        (if (good_enough? guess) 
            guess
            (iter (improve guess))))
    (lambda (guess) (iter guess)))


(define (sqrt_improve x)
    ((iterative_improve 
        (lambda (guess) (< (abs (- (expt guess 2) x)) 0.001))
        (lambda (guess) (average guess (/ x guess))))
    1.0))

(define (fixed_point_improve fn guess)
    ((iterative_improve
        (lambda (guess) (< (abs (- (fn guess) guess)) 0.001))
        fn)
    guess))



(display "===[ END ]===\n")
; (exit)
