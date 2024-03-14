#lang racket

(define =zero? void)
(define put void)
(define get void)
(define (attach-type type-tag contents)
  (cons type-tag contents))

;; These are the same for both packages
(define empty-termlist '())
(define (empty-termlist? term-list) (null? term-list))
(define (rest-terms term-list) (cdr term-list))
(define (order term) (car term))
(define (coeff term) (cdr term))
(define (make-term order coeff) (cons order coeff))


(define (install-dense-term-lists)
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

    (define (tag arg) (attach-type 'dense-term-list arg))
    (put 'adjoin-term 'dense-term-list
         (lambda (term term-list) (tag (adjoin-term term term-list))))
    (put 'first-term 'dense-term-list
         (lambda (term-list) (tag (first-term term-list))))
    (put 'make 'dense-term-list
         (lambda (order coeff) (tag (make-term order coeff))))
    
    'done)

(define (install-sparse-term-lists)
  (define (adjoin-term term term-list)
    ;; Needs to be called with 'term' of a higher order
    ;; than term-list's max order.
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (first-term term-list) (car term-list))

  (define (tag arg) (attach-type 'sparse-term-list arg))
  (put 'adjoin-term 'sparse-term-list
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'first-term 'sparse-term-list
       (lambda (term-list) (tag (first-term term-list))))
  (put 'make 'sparse-term-list
       (lambda (order coeff) (tag (make-term order coeff))))

  'done)

;; Generic operations
(define (adjoin-term term term-list)
  (get 'adjoin-term term term-list))

(define (first-term term-list)
  (get 'first-term term-list))
