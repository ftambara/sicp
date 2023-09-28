#lang racket

(require rackunit)


;; From the book

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (number? num) (= exp num)))

; (define (derive expr variable)
;   (cond ((number? expr) 0)
;         ((variable? expr)
;          (if (same-variable? expr variable) 1 0))
;         ((sum? expr)
;          (make-sum (derive (addend expr) variable)
;                    (derive (augend expr) variable)))
;         ((product? expr)
;          (let ((m1 (multiplier expr))
;                (m2 (multiplicand expr)))
;            (make-sum (make-product m1 (derive m2 variable))
;                      (make-product (derive m1 variable) m2))))
;         (else (error "Cannot derive " expr))))

;; Solutions

; a. Fully parenthesized expressions

; (define (make-sum a1 a2)
;   (cond ((=number? a1 0) a2)
;         ((=number? a2 0) a1)
;         ((and (number? a1) (number? a2)) (+ a1 a2))
;         (else (list a1 '+ a2))))

; (define (sum? expr)
;   (and (list? expr) (eq? (cadr expr) '+)))

; (define (addend sum)
;   (car sum))

; (define (augend sum)
;   (caddr sum))

; (define (make-product m1 m2)
;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;         ((=number? m1 1) m2)
;         ((=number? m2 1) m1)
;         ((and (number? m1) (number? m2)) (* m1 m2))
;         (else (list m1 '* m2))))

; (define (product? expr)
;   (and (list? expr) (eq? (cadr expr) '*)))

; (define (multiplier prod)
;   (car prod))

; (define (multiplicand prod)
;   (caddr prod))


; (check-equal? (derive '(x + 3) 'x) 1)
; (check-equal? (derive '(x * y) 'x) 'y)
; (check-equal?  (derive '((x * y) * (x + 3)) 'x)
;                '((x * y) + (y * (x + 3))))


; b. Standard algebraic notation (except for outer parenthesis)

(define (addend sum)
  (let* ((plus-index (index-of sum '+))
         (augend-list (take sum plus-index)))
    (if (= (length augend-list) 1)
        (car augend-list)
        augend-list)))

(define (augend sum)
  (let* ((plus-index (index-of sum '+))
         (augend-list (drop sum (add1 plus-index))))
    (if (= (length augend-list) 1)
        (car augend-list)
        augend-list)))

(check-equal? (addend '(x + y * 3)) 'x)
(check-equal? (addend '(x * y + 3)) '(x * y))

(check-equal? (augend '(x + y * 3)) '(y * 3))
(check-equal? (augend '(x + 3)) 3)
(check-equal? (augend '(x + y + 3)) '(y + 3))

(define (member? item list)
  (cond ((null? list) #f)
        ((eq? item (car list)) #t)
        (else (member? item (cdr list)))))

(define (sum? expr)
  (and (list? expr) (member? '+ expr)))

(check-true (sum? '(x + y)))
(check-false (sum? '(x * y)))
(check-false (sum? '(x * (y + 3))))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (sum? a1) (sum? a2)) (append a1 '(+) a2))
        ((sum? a1) (append a1 (list '+ a2)))
        ((sum? a2) (append (list a1 '+) a2))
        (else (list a1 '+ a2))))

(check-equal? (make-sum 3 2) 5)
(check-equal? (make-sum 'x 0) 'x)
(check-equal? (make-sum 'x 'y) '(x + y))
(check-equal? (make-sum '(x * 2) 'y) '((x * 2) + y))
(check-equal? (make-sum 'x '(2 * y)) '(x + (2 * y)))
(check-equal? (make-sum '(x + 2) 'y) '(x + 2 + y))
(check-equal? (make-sum 'x '(2 + y)) '(x + 2 + y))
(check-equal? (make-sum '(x + 2) '(y + z)) '(x + 2 + y + z))

(define (product? expr)
  (and (list? expr) (member? '* expr)))

(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (product? m1) (product? m2)) (append m1 '(*) m2))
        ((product? m1) (append m1 (list '* m2)))
        ((product? m2) (append (list m1 '*) m2))
        (else (list m1 '* m2))))

(check-equal? (make-product 3 2) 6)
(check-equal? (make-product 'x 0) 0)
(check-equal? (make-product '(x + y) 1) '(x + y))
(check-equal? (make-product 'x 'y) '(x * y))
(check-equal? (make-product '(x + 2) 'y) '((x + 2) * y))
(check-equal? (make-product 'x '(2 + y)) '(x * (2 + y)))
(check-equal? (make-product '(x * 2) 'y) '(x * 2 * y))
(check-equal? (make-product 'x '(2 * y)) '(x * 2 * y))
(check-equal? (make-product '(x * 2) '(y * z)) '(x * 2 * y * z))

(define (multiplier product)
  (let* ((times-index (index-of product '*))
         (mult-list (take product times-index)))
    (if (= (length mult-list) 1)
        (car mult-list)
        mult-list)))

(define (multiplicand product)
  (let* ((times-index (index-of product '*))
         (mult-list (drop product (add1 times-index))))
    (if (= (length mult-list) 1)
        (car mult-list)
        mult-list)))

(check-equal? (addend '(x + y * 3)) 'x)
(check-equal? (addend '(x * y + 3)) '(x * y))

(check-equal? (augend '(x + y * 3)) '(y * 3))
(check-equal? (augend '(x + 3)) 3)
(check-equal? (augend '(x + y + 3)) '(y + 3))


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
        (else (error "Cannot derive " expr))))

(check-equal? (derive '(x + 3) 'x) 1)
(check-equal? (derive '(x * y) 'x) 'y)
(check-equal?  (derive '(x * y * (x + 3)) 'x)
               '((x * y) + (y * (x + 3))))
(check-equal? (derive '(x + 3 * (x + y + 2)) 'x) 4)
(check-equal? (derive '(x * 3 * (x + y + 2)) 'x)
              '((x * 3) + (3 * (x + y + 2))))
