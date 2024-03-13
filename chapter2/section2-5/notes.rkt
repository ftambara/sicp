#lang racket

(require "../section2-4/notes.rkt")


;; Generic Arithmetic Operations

;; Doesn't previous section's apply-generic procedure already solve
;; the problem of dispatching on different arguments?
;;  Indeed it does, this section is to show how that looks like.
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (put op types fn)
  (display "Implementation pending"))

(define (get op types)
  (display "Implementation pending"))

(define (install-scheme-number-package)
  ;; Handle ordinary numbers
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

;; The system can't handle literals without tags
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(define (install-rational-package)
  ;; Rational numbers
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-polar-package)
(install-rectangular-package)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


;; Mixed Types

;; to be included in the complex package
;; Why not on the scheme-number package?
(define make-from-real-imag void)
(define tag void)

(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x) (imag-part z)))
(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

;; This variation is also needed
(define (add-schemenum-to-complex x z)
  (make-from-real-imag (+ (real-part z) x) (imag-part z)))
(put 'add '(complex scheme-number)
     (lambda (x z) (tag (add-complex-to-schemenum x z))))

;; If our packages naturally need a lot of cross-type interaction,
;; isn't that a clue that we should bundle them all together in a
;; single package? Having two-way dependencies between otherwise
;; independent packages is strange.

;; Coercion solves the problem of excessive definition of procedures,
;; but it does eliminate two-way dependencies.

(define make-complex-from-real-imag void)
(define put-coercion void)
(define get-coercion void)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; The book mentions a clever way to look for unspecified coercions
;; by building a chain from the existing coercion procedures.
;; But to me that sounds terrible for performance. Is that so?

;; Symbolic Algebra

(define (install-polynomial-package)
  ;; internal procedures

  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable poly) (car poly))
  (define (term-list poly) (cdr poly))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? e)
    (symbol? e))

  ;; representation of terms and term lists
  (define empty-termlist '())
  (define (adjoin-term term term-list)
    ;; Needs to be called with 'term' of a higher order
    ;; than term-list's max order.
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (make-term order coeff) (cons order coeff))
  (define (order term) (car term))
  (define (coeff term) (cdr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  ;; procedures used by add-poly
  (define (add-terms tl1 tl2)
    (cond ((empty-termlist? tl1) tl2)
          ((empty-termlist? tl2) tl1)
          (else 
            (let ((t1 (first-term tl1))
                  (t2 (first-term tl2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms tl1) tl2)))
                    ((< (order t1) (order t2))
                     (adjoin-term t2 (add-terms tl1 (rest-terms tl2))))
                    (else 
                      (adjoin-term
                        ;; Use generic add to allow coefficients
                        ;; to be of any type
                        (make-term (order t1) (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms tl1) 
                                   (rest-terms tl2)))))))))


  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  ;; procedures used by mul-poly
  (define (mul-terms tl1 tl2)
    (if (empty-termlist? tl1)
      empty-termlist
      (add-terms (mul-term-by-all-terms (first-term tl1) tl2)
                 (mul-terms (rest-terms tl1) tl2))))

  (define (mul-term-by-all-terms t1 tl)
    (if (empty-termlist? tl)
      empty-termlist
      (let ((t2 (first-term tl)))
        (adjoin-term (make-term (+ (order t1) (order t2)) 
                                ;; Generic mul
                                (mul (coeff t1) (coeff t2)))
                     (mul-term-by-all-terms t1 (rest-terms tl))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
