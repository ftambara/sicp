#lang racket


(define put-coercion void)
(define get-coercion void)
(define put void)
(define get void)
(define tag void)
;; Defined in section 2.5 notes
(define type-tag void)
(define contents void)

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; a.
(define (exp x y) (apply-generic 'exp x y))

;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     ; using primitive expt
     (lambda (x y) (tag (expt x y))))

;; Without Louis's procedures, what would happen if exp was called
;; with two complex numbers is: first, no procedure would be found
;; with (get 'exp '(complex complex)); apply-generic would then try
;; to find a complex->complex procedure two times, one as t1->t2 and
;; another as t2->t1, and fail; finally, apply-generic would throw an
;; error. This is ok, since complex numbers are usually not coercible
;; to real numbers.

;; With Louis's no-op procedures, both t1->t2 and t2->t1 coercion
;; procedures would exist, but (get 'exp '(complex complex)) would
;; not find any procedure (we already tested for that in the beginning
;; of apply-generic).


;; b.
;; Clearly the procedures don't help, they only add extra work and
;; don't prevent any errors.

;; c.
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
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                         (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))
