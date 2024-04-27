#lang racket

;; With delay we decouble the apparent order of events, as expressed
;; in the program, from the actual order of events as they occur in
;; time.

(require
  "../modules/streams.rkt"
  math/number-theory
  rackunit)

(define (sum-of-primes start end)
  (define (iter count accum)
    (cond [(>= count end) accum]
          [(prime? count) (iter (add1 count)
                                (+ count accum))]
          [else (iter (add1 count) accum)]))
  (iter start 0))

;; (displayln "Traditional sum-of-primes:")
;; (time (sum-of-primes 1 1000000))
;; cpu time: 210 real time: 211 gc time: 75

(define (sum-of-primes-seq start end)
  (foldl +
         0
         (filter prime? (enum-range start end))))

(define (enum-range start end)
  (if (>= start end)
    '()
    (cons start (enum-range (add1 start) end))))

;; (displayln "Checking sequence-style sum-of-primes")
;; (time (sum-of-primes-seq 1 1000000))
;; cpu time: 255 real time: 256 gc time: 118

;; stream-cons is a special form, because the second argument
;; must be interpreted as a body and not be evaluated
;; the equivalence to (cons a (delay b))*, then, is not realizable
;; by a traditional define.
;; *: Visualizing delay as a thunk helps visualizing the problem
;;  (cons a (lambda () b))

;; Sums are not where the streams will show a speed advantage,
;; I've written it out of curiosity
(define (sum-of-primes-stream start end)
  (stream-fold
    +
    0
    (stream-filter prime? (stream-enumerate start end))))

(define (stream-enumerate start end)
  (if (>= start end)
    the-empty-stream
    (stream-cons start
                 (stream-enumerate (add1 start) end))))

;; (displayln "Checking sum-of-primes stream")
;; (time (sum-of-primes-stream 1 1000000))
;; cpu time: 608 real time: 610 gc time: 46
;; really slow

;; Calculate times of getting the second prime in the range
(displayln "Traditional style second prime")
(define (prime-ref nth start end)
  (cond [(>= start end) (error "No primes in range")]
        [(prime? start)
         (if (= nth 1)
           start
           (prime-ref (sub1 nth) (add1 start) end))]
        [else (prime-ref nth (add1 start) end)]))
(time (prime-ref 2 1000000 10000000))
;; cpu time: 9 real time: 10 gc time: 1

;; (displayln "Checking sequence-style second prime in range")
;; (time (cadr (filter prime? (enum-range 1000000 10000000))))
;; cpu time: 11033 real time: 11047 gc time: 426

#|
(displayln "Checking stream-style second prime in range")
(time
  (stream-first
    (stream-rest
      (stream-filter prime? (stream-enumerate 1000000 10000000)))))
|#
;; cpu time: 0 real time: 0 gc time: 0

#|
From the book:

(cons-stream (stream-car stream)
             (stream-filter pred (stream-cdr stream)))

which in this case is

(cons 10007
      (delay (stream-filter
               prime?
               (cons 10008
                     (delay (stream-enumerate-interval
                              10009
                              1000000))))))

In the same way that it was said that variables are not actually
replaced by their values, but their environments are saved instead, I
think it's correct to say that the output of the stream-filter is:

(cons 10007
      (delay (stream-filter pred (stream-cdr stream))))

Where pred is prime?, stream is (stream-enumerate-interval (+ low 1) high),
low is 10008 and high is 1000000.
|#
