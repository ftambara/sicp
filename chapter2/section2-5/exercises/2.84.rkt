#lang racket

(require "2.83.rkt")

(define get void)
(define get-coercion void)


(define (raise-until-equal from to)
  (let* ((type1 (type-tag from))
         (type2 (type-tag to))
         (raise (get 'raise type1)))
    (and raise
         (let* ((from-raised (raise from))
                (type1-raised (type-tag from-raised)))
           (or (equal? type1-raised type2)
               (raise-until-equal from-raised to))))))


(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let* ((arg1 (car args))
                   (arg2 (cadr args))
                   (arg1-raised (raise-until-equal arg1 arg2))
                   (arg2-raised (raise-until-equal arg2 arg1)))
              (cond (arg1-raised (apply-generic op arg1-raised arg2))
                    (arg2-raised (apply-generic op arg1 arg2-raised))
                    (else (error "No method for these types"
                                 (list op type-tags)))))
            (error "No method for these types"
                   (list op type-tags))))))

