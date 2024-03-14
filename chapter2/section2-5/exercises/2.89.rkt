#lang racket

(define =zero? void)

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (let* ((fst-term (first-term term-list))
          (order-fst (order fst-term)))
      (cond ((= (order term) order-fst)
             (if (not (=zero? (coeff fst-term)))
               (error "non-zero term with same order already exists")
               (cons (coeff term) (rest-terms term-list))))
            ((< (order term) order-fst)
             (cons fst-term (adjoin-term term (rest-terms term-list))))
            ((> (order term) order-fst)
             (adjoin-term term (cons 0 term-list)))))))

(define (first-term term-list)
  (make-term (- (length term-list) 1)
             (car term-list)))

;; Stay the same
(define empty-termlist '())
(define (empty-termlist? term-list) (null? term-list))
(define (rest-terms term-list) (cdr term-list))
(define (make-term order coeff) (cons order coeff))
(define (order term) (car term))
(define (coeff term) (cdr term))
