#lang racket

(require
  "3.26.rkt"
  rackunit)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (cmp-numbers a b)
  (cond ((< a b) -1)
        ((= a b) 0)
        ((> a b) 1)))

(define (memoize f)
  (let ((table (make-table cmp-numbers)))
    (lambda (x)
      (let ((previously-computed-result (table 'lookup (list x))))
        (if previously-computed-result
          (printf "found ~a in table\n" x)
          (display "X\n"))
        (or previously-computed-result
            (let ((result (f x)))
              (table 'insert! (list x) result)
              result))))))

(define memo-fib
  (memoize
    (lambda (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (memo-fib (- n 1))
                     (memo-fib (- n 2))))))))

(memo-fib 6)

;; 1. Draw an environment diagram to analyze the computation of (memo-fib 3)
;; Assuming left-to-right evaluation order. Solution is in 3.27.png

;; 2. Explain why memo-fib computes the n-th Fibonacci number 
;;  in a number of steps proportional to n

;; After computing (memo-fib 2), all larger Fibonacci numbers require
;; only one new computation plus one table lookup. Thus, an increase
;; of one in the input results in two new computations, which is O(n).
;; That is if we ignore table lookup time, which may be O(n^2) in the book's
;; implementation.

;; 3. Would the scheme still work if we had simply deﬁned 
;;  memo-fib to be (memoize fib)?

;; No, because recursive calls to fib would not be memoized.
