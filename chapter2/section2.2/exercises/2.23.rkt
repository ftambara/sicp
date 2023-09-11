#lang racket

(provide for-each)


(define (for-each procedure elements)
  (cond ((null? elements) null)
        (else
         (procedure (car elements))
         (for-each procedure (cdr elements)))))
