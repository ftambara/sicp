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
;; FRAME I
;; n = 6
;;
;; (= 6 1) -> false
;; -> else
;; (* 6 (factorial-rec 5))
;;
;; FRAME II
;; n = 5
;;
;; (= 5 1) -> false
;; -> else
;; (* 5 (factorial-rec 4))
;;
;; FRAME III
;; n = 4
;;
;; (= 4 1) -> false
;; -> else
;; (* 4 (factorial-rec 3))
;;
;; FRAME IV
;; n = 3
;;
;; (= 3 1) -> false
;; -> else
;; (* 3 (factorial-rec 2))
;;
;; FRAME V
;; n = 2
;;
;; (= 2 1) -> false
;; -> else
;; (* 2 (factorial-rec 1))
;;
;; FRAME VI
;; n = 1
;;
;; (= 1 1) -> true
;; -> then
;; 1
;;
;; (* 2 1) -> 2
;; (* 3 2) -> 6
;; (* 4 6) -> 24
;; (* 5 24) -> 120
;; (* 6 120) -> 720


;; Evaluation of (factorial-iter 6)

;; (factorial-iter 6)
;; FRAME I
;; n = 6
;;
;; (fact-iter 1 1 6)
;;
;; FRAME II
;; product = 1
;; counter = 1
;; max-count = 6
;;
;; (> 1 6) -> false
;; -> else
;; (fact-iter (* 1 1) (+ 1 1) 6)
;; (fact-iter 1 2 6)
;;
;; FRAME III
;; product = 1
;; counter = 2
;; max-count = 6
;;
;; (> 2 6) -> false
;; -> else
;; (fact-iter (* 2 1) (+ 2 1) 6)
;; (fact-iter 2 3 6)
;;
;; FRAME IV
;; product = 2
;; counter = 3
;; max-count = 6
;;
;; (> 3 6) -> false
;; -> else
;; (fact-iter (* 3 2) (+ 3 1) 6)
;; (fact-iter 6 4 6)
;;
;; FRAME V
;; product = 6
;; counter = 4
;; max-count = 6
;;
;; (> 4 6) -> false
;; -> else
;; (fact-iter (* 4 6) (+ 4 1) 6)
;; (fact-iter 24 5 6)
;;
;; FRAME VI
;; product = 24
;; counter = 5
;; max-count = 6
;;
;; (> 5 6) -> false
;; -> else
;; (fact-iter (* 5 24) (+ 5 1) 6)
;; (fact-iter 120 6 6)
;;
;; FRAME VII
;; product = 120
;; counter = 6
;; max-count = 6
;;
;; (> 6 6) -> false
;; -> else
;; (fact-iter (* 6 120) (+ 6 1) 6)
;; (fact-iter 720 7 6)
;;
;; FRAME VIII
;; product = 720
;; counter = 7
;; max-count = 6
;; (> 7 6) -> true
;; -> then
;; 720
