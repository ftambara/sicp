#lang racket

(require "2.56.rkt"
         rackunit)


(define (zero-num? elem)
  (and (number? elem) (zero? elem)))

(define (any? pred elements)
  (if (null? elements)
      false
      (or (pred (car elements)) (any? pred (cdr elements)))))

(define not-a-number?
  (lambda (elem) (not (number? elem))))

(define (build-op op expressions)
  (if (= (length expressions) 1)
      (car expressions)
      (cons op expressions)))

(define (make-sum addend augend)
  (define (sum-numbers terms)
    (foldl (lambda (term accu) (if (number? term) (+ term accu) accu))
           0
           terms))
  (let* ((augend-list (if (sum? augend) (cdr augend) (list augend)))
         (terms (cons addend augend-list)))
    (build-op '+ (remove 0 (append (filter not-a-number? terms)
                                   (list (sum-numbers terms)))))))

(define (augend sum)
  (if (= (length sum) 3)
      (caddr sum)
      (make-sum (caddr sum) (cadddr sum))))

(define (make-product mul1 mul2)
  (define (multiply-numbers factors)
    (foldl (lambda (factor accu) (if (number? factor) (* factor accu) accu))
           1
           factors))

  (let* ((mul2-list (if (product? mul2) (cdr mul2) (list mul2)))
         (factors (cons mul1 mul2-list)))
    (if (any? zero-num? factors)
        0
        (build-op '* (remove 1 (cons (multiply-numbers factors)
                                     (filter not-a-number? factors)))))))

(define (multiplicand prod)
  (if (= (length prod) 3)
      (caddr prod)
      (make-product (caddr prod) (cadddr prod))))

(define (derive expr variable)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr variable) 1 0))
        ((sum? expr)
         (make-sum (derive (addend expr) variable)
                   (derive (augend expr) variable)))
        ((product? expr)
         (let ((m1 (multiplier expr))
               (m2 (multiplicand expr)))
           (make-sum (make-product m1 (derive m2 variable))
                     (make-product (derive m1 variable) m2))))
        ;; Solution
        ((and (exponentiation? expr) (number? (exponent expr)))
         (let ((expr-base (base expr))
               (expr-exponent (exponent expr)))
           (make-product (make-product expr-exponent
                                       (make-exponentiation expr-base (- expr-exponent 1)))
                         (derive expr-base variable))))
        (else (error "Cannot derive " expr))))

(check-equal? (make-sum 'x '(+ 3 y 2 0 1)) '(+ x y 6))
(check-equal? (make-sum 'x 3) '(+ x 3))
(check-equal? (make-sum 3 'x) '(+ x 3))
(check-equal? (make-sum 'x 0) 'x)
(check-equal? (make-sum 3 4) 7)

(check-equal? (make-product 'x '(* 3 y 2 1)) '(* 6 x y))
(check-equal? (make-product 'x 3) '(* 3 x))
(check-equal? (make-product 'x 1) 'x)
(check-equal? (make-product 2 3) 6)

(check-equal? (derive '(* x y (+ x 3)) 'x)
              '(+ (* x y) (* y (+ x 3))))

;; Notes: The implementation is rather awkward. I might have been able
;; to do a better job if I used the equivalent of Python's star
;; unpacking. I also complicated things a bit by exploring the
;; accumulation of numbers at the end of sums and at the beginning of
;; products.
;; Overall, the mission of adding new functionality without changing
;; the derive procedure (which was my first instinct) was
;; accomplished.