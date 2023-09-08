#lang racket

(provide
    right-split
    up-split)


(define (split comp1 comp2)
    (define (split-n painter n)
        (let ((sub-split (comp2 painter painter)))
            (if (= n 0)
                painter
                (split-n (comp1 painter sub-split) (- n 1)))))
    split-n)

;; To be defined later.
(define (beside painter1 painter2)
    (display "beside"))

(define (below painter1 painter2)
    (display "below"))
;;;;

(define right-split (split beside below))
(define up-split (split below beside))
