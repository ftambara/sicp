#lang racket

(define add-poly void)

(define (negate num)
  (apply-generic 'negate num))

;; example negate definition for another type
(put 'negate 'rational
     (lambda (r)
       (attach-type 
         'rational
         (make-rat (negate (num r)) (denom r)))))

;; to be included in poly package
(define (sub-poly p1 p2)
  (add-poly p1 (negate p2)))

(define (negate-terms tl)
  (let ((ft (firt-term tl)))
      (adjoin-term (make-term (order ft) (negate (coeff ft)))
                   (rest-terms tl))))

(put 'negate 'polynomial
     (lambda (p)
       (make-poly (variable p) (negate-terms (term-list p)))))
