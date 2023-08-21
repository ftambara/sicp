#lang racket


(newline)
(display "===[ START ]===\n")

(define (print x)
    (display x)
    (newline)
)
(define true #t)
(define false #f)

(define (avg a b)
    (/ (+ a b) 2)
)

(define (square num)
    (* num num)
)

(define (largest n1 n2)
    (if (> n1 n2)
        n1
        n2
    )
)

(define (f1-3 num1 num2 num3)
    (if (and (> num1 num2) (> num1 num3))
        (+ (square num1) (square (largest num2 num3)))
        (if (and (> num2 num1) (> num2 num3))
            (+ (square num2) (square (largest num1 num3)))
            (+ (square num3) (square (largest num1 num2)))
        )
    )
)

(print (f1-3 3 5 2))

(define (abs num)
    ((if (< num 0) - +) num)
)

(define (good-enough? guess prev tol)
    (and (not (= prev 0)) (< (/ (abs (- guess prev)) guess) tol))
)

(define (sqrt-nr num guess prev tol)
    (if (good-enough? guess prev tol)
        guess
        (sqrt-nr num (avg guess (/ num guess)) guess tol)
    )
)

(define (sqrt num) (sqrt-nr num 1.0 0 0.1))

(define (cubert num)

    (define (improve-cubert-guess guess)
        (/ (+ (/ num (square guess))
            (* 2 guess)) 
            3)
    )

    (define (cubert-iter guess prev tol)
        (if (good-enough? guess prev tol)
            guess
            (cubert-iter (improve-cubert-guess guess) guess tol)
        )
    )

    (cubert-iter 1.0 0 0.1)
)


(display "===[ END ]===\n")
; (exit)