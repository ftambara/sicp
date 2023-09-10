#lang racket

(require "2.50.rkt")

(provide below
         beside)


(define (below painter1 painter2)
  (let ((bottom-painter
         (transform-painter painter1
                            (make-vector 0.0 0.0)
                            (make-vector 1.0 0.0)
                            (make-vector 0.0 0.5)))
        (top-painter
         (transform-painter painter2
                            (make-vector 0.0 0.5)
                            (make-vector 1.0 0.5)
                            (make-vector 0.0 1.0))))
    ((lambda (frame)
       (bottom-painter frame)
       (top-painter frame)))))

; From the book, with modifications
(define (beside painter1 painter2)
  (let ((left-painter
         (transform-painter painter1
                            (make-vector 0.0 0.0)
                            (make-vector 0.5 0.0)
                            (make-vector 0.0 1.0)))
        (right-painter
         (transform-painter painter2
                            (make-vector 0.5 0.0)
                            (make-vector 1.0 0.0)
                            (make-vector 0.5 1.0))))
    ((lambda (frame)
       (left-painter frame)
       (right-painter frame)))))
