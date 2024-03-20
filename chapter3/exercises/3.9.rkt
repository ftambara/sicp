#lang racket

(define (factorial-rec n)
  (if (= n 1) 1 (* n (factorial-rec (- n 1)))))


(define (factorial-iter n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

;; Evaluation of (factorial-rec 6)

;; (factorial-rec 6)
;;       ┌─FRAME I─┐
;; E1 -> │ n = 6   │ -> GLOBAL ENVIRONMENT
;;       └─────────┘
;;
;; (* 6 (factorial-rec 5))
;;       ┌─FRAME II─┐
;; E2 -> │ n = 5    │ -> GLOBAL ENVIRONMENT
;;       └──────────┘
;;
;; (* 5 (factorial-rec 4))
;;       ┌─FRAME III─┐
;; E3 -> │ n = 4     │ -> GLOBAL ENVIRONMENT
;;       └───────────┘
;;
;; (* 4 (factorial-rec 3))
;;       ┌─FRAME IV─┐
;; E4 -> │ n = 4    │ -> GLOBAL ENVIRONMENT
;;       └──────────┘
;;
;; (* 3 (factorial-rec 2))
;;       ┌─FRAME V─┐
;; E5 -> │ n = 2   │ -> GLOBAL ENVIRONMENT
;;       └─────────┘
;;
;; (* 2 (factorial-rec 1))
;;       ┌─FRAME VI─┐
;; E6 -> │ n = 1    │ -> GLOBAL ENVIRONMENT
;;       └──────────┘
;;
;; 1
;; (* 2 1)
;; (* 3 2)
;; (* 4 6)
;; (* 5 24)
;; (* 6 120)
;; 720


;; Evaluation of (factorial-iter 6)

;; (factorial-iter 6)
;;       ┌─FRAME I─┐
;; E1 -> │ n = 6   │ -> GLOBAL ENVIRONMENT
;;       └─────────┘
;;
;; (fact-iter 1 1 6)
;;       ┌───FRAME II────┐
;; E2 -> │ product = 1   │ -> GLOBAL ENVIRONMENT
;;       │ counter = 1   │
;;       │ max-count = 6 │
;;       └───────────────┘
;;
;; (fact-iter 1 2 6)
;;       ┌───FRAME III───┐
;; E3 -> │ product = 1   │ -> GLOBAL ENVIRONMENT
;;       │ counter = 2   │
;;       │ max-count = 6 │
;;       └───────────────┘
;;
;; (fact-iter 2 3 6)
;;       ┌───FRAME IV────┐
;; E4 -> │ product = 2   │ -> GLOBAL ENVIRONMENT
;;       │ counter = 3   │
;;       │ max-count = 6 │
;;       └───────────────┘
;;
;; (fact-iter 6 4 6)
;;       ┌────FRAME V────┐
;; E5 -> │ product = 6   │ -> GLOBAL ENVIRONMENT
;;       │ counter = 4   │
;;       │ max-count = 6 │
;;       └───────────────┘
;;
;; (fact-iter 24 5 6)
;;       ┌────FRAME V────┐
;; E6 -> │ product = 24  │ -> GLOBAL ENVIRONMENT
;;       │ counter = 5   │
;;       │ max-count = 6 │
;;       └───────────────┘
;;
;; (fact-iter 120 6 6)
;;       ┌───FRAME VI────┐
;; E7 -> │ product = 120 │ -> GLOBAL ENVIRONMENT
;;       │ counter = 6   │
;;       │ max-count = 6 │
;;       └───────────────┘
;;
;; (fact-iter 720 7 6)
;;       ┌───FRAME VII───┐
;; E8 -> │ product = 720 │ -> GLOBAL ENVIRONMENT
;;       │ counter = 7   │
;;       │ max-count = 6 │
;;       └───────────────┘
;;
;; 720
