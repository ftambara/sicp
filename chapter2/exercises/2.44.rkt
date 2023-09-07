#lang racket

(provide up-split)


(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller)))))

;; To be defined later.
(define (beside painter1 painter2)
    (display "beside"))

(define (below painter1 painter2)
    (display "below"))
