#lang racket

(define f
  (let ((prod 1))
    (lambda (n)
        (set! prod (* prod n))
        prod)))


(+ (f 0) (f 1))
;; Left to right
;; (f 0) -> 0
;; (f 1) -> 0
;; (+ 0 0) -> 0
;;
;; Right to left
;; (f 1) -> 1
;; (f 0) -> 0
;; (+ 1 0) -> 1

;; In racket, result is 0. Therefore, Racket
;; uses left to right evaluation
