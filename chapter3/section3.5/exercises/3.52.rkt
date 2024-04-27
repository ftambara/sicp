#lang racket

(require "../../modules/streams.rkt")

;; > Consider the sequence of expressions

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map-book accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter-book even? seq))
(define z
  (stream-filter-book (lambda (x) (= (remainder x 5) 0))
                 seq))
(stream-ref-book y 7)
(stream-display z)

;; > What is the value of sum after each of the above expressions
;; is evaluated?

#|
(define sum 0)
sum = 0

(define (accum x) (set! sum (+ x sum)) sum)

(define seq
  (stream-map-book accum
                   (stream-enumerate-interval 1 20)))
seq = (cons (accum 1)
            (delay (stream-map-book
                     accum
                     (stream-enumerate-interval 2 20))))
sum = 1
seq = (cons 1
            (delay (stream-map-book
                     accum
                     (stream-enumerate-interval 2 20))))

(define y (stream-filter-book even? seq))
Evaluate seq until (even? sum) is true (sum == 6)
sum = 1 + (2 + 3) = 6
seq = (list* 1 3 6
            (delay (stream-map-book
                     accum
                     (stream-enumerate-interval 4 20))))
Note: seq pointer hasn't changed. What I represent here are cached results,
which mean that those expressions won't be evaluated again
y = (cons 6
          (delay (stream-filter-book
                   even?
                   (delay (stream-map-book
                            accum
                            (stream-enumerate-interval 4 20))))))

(define z
  (stream-filter-book (lambda (x) (= (remainder x 5) 0))
                      seq))
Evaluate seq until (= (remainder sum 5) 0) is true (sum == 10)
sum = 6 + 4 = 10
seq = (list* 1 3 6 10
            (delay (stream-map-book
                     accum
                     (stream-enumerate-interval 5 20))))
z: (cons 10
         (delay (stream-filter-book
                  (lambda (x) (= (remainder x 5) 0))
                  (delay (stream-map-book
                           accum
                           (stream-enumerate-interval 5 20))))))

(stream-ref-book y 7)
sum = 10 + (5 + 6 + ... + 15 + 16) = 136
y = (list* 6 10 28 36 66 78 120 136
          (delay (stream-filter-book
                   even?
                   (delay (stream-map-book
                            accum
                            (stream-enumerate-interval 17 20))))))
seq = (list* 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136
            (delay (stream-map-book
                     accum
                     (stream-enumerate-interval 17 20))))
sum = 136

Result 136


(stream-display z)
stream-display forces the evaluation of the whole stream
seq = (list* 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
sum = 210
z = (list* 10 15 45 55 105 120 190 210)
|#

;; > What is the printed response to evaluating
;; the stream-ref and display-stream expressions?
;; (stream-ref-book y 7)
;; Returns 126

;; (stream-display z)
;; Prints:
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210
;; Returns 'done

;; > Would these responses diﬀer if we had implemented (delay ⟨exp⟩)
;; simply as (lambda () ⟨exp⟩) without using the optimization provided by
;; memo-proc? Explain.

;; If evaluated delayed expressions wouldn't have been memoized, every
;; evaluation of seq would start with a different sum value, which would
;; alter all subsequent results
