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
;; (displayln "Traditional style second prime")
(define (prime-ref nth start end)
  (cond [(>= start end) (error "No primes in range")]
        [(prime? start)
         (if (= nth 1)
           start
           (prime-ref (sub1 nth) (add1 start) end))]
        [else (prime-ref nth (add1 start) end)]))
;; (time (prime-ref 2 1000000 10000000))
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

;; Erastothenes sieve

(define (integers-starting-from num)
  (stream-cons num (integers-starting-from (add1 num))))

(define (divisible? a b)
  (= (remainder a b) 0))

(define (sieve stream)
  (stream-cons (stream-first stream)
               (sieve (stream-filter
                        (lambda (x)
                          (not (divisible? x (stream-first stream))))
                        (stream-rest stream)))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)

(define (stream-map-all proc . argstreams)
  (if (null? (car argstreams))
    empty-stream
    (stream-cons
      (apply proc (map stream-first argstreams))
      (apply stream-map-all proc (map stream-rest argstreams)))))

(define (add-streams s1 s2) (stream-map-all + s1 s2))
(define fib-stream
  (stream-cons 0
               (stream-cons 1
                            (add-streams
                              fib-stream
                              (stream-cdr fib-stream)))))

(stream-ref fib-stream 2)
(stream-ref fib-stream 3)
(stream-ref fib-stream 4)
(stream-ref fib-stream 5)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* factor x))
              stream))

;; Using streams
(define (average . nums)
  (/ (foldl + 0 nums) (length nums)))

(define (sqrt-improve guess x)
  (exact->inexact (average guess (/ x guess))))

;; My version
(define (sqrt-stream x)
  (define (guess-stream last-guess)
    (stream-cons last-guess (guess-stream (sqrt-improve last-guess x))))
  (guess-stream 1))

;; The book's version
(define (sqrt-stream-book x)
  (define guesses
    (stream-cons
      1
      ;; This definition depends on memoization to work
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)

(define (pi-summands n)
  (stream-cons (/ n) (stream-map - (pi-summands (+ n 2)))))

(define (partial-sums s)
   (add-streams s (stream-cons 0 (partial-sums s))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ([s0 (stream-ref s 0)]   ; S_(n-1)
        [s1 (stream-ref s 1)]   ; S_n
        [s2 (stream-ref s 2)])  ; S_(n+1)
    (stream-cons (- s2 (/ (expt (- s2 s1) 2)
                          (+ s0 (- (* 2 s1)) s2)))
                 (euler-transform (stream-rest s)))))

;; make-tableau produces a stream of incrementally transformed streams
(define (make-tableau transform s)
  (stream-cons s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-first (make-tableau transform s)))

(define integers (stream-cons 1 (stream-map add1 integers)))

(define (pairs s t)
  (let ([s0 (stream-first s)])
    (stream-cons
      (cons s0 (stream-first t))
      (interleave
        (stream-map (lambda (ti) (cons s0 ti)) (stream-rest t))
        (pairs (stream-rest s) (stream-rest t))))))

(define int-pairs (pairs integers integers))

(stream-filter
  (lambda (pair) (prime? (+ (car pair) (cadr pair))))
  int-pairs)

;; For an interleaving process to be correct, any chosen element must appear
;; if we select enough items from the resulting stream
(define (interleave s1 s2)
  (if (stream-empty? s1)
    s2
    (stream-cons (stream-first s1)
                 (interleave s2 (stream-rest s1)))))

;; Streams as signals
(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 ;; Why is integrand scaled by dt?
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)


;; Delayed evaluation

(define (delayed-integral delayed-integrand initial-value dt)
  (define int
    (stream-cons
      initial-value
      (let ((integrand (force delayed-integrand)))
        (add-streams (scale-stream integrand dt) int))))
        ;; Why define integrand here, if it can be done in-line?
  int)

(define (solve f y0 dt)
  (define y (delayed-integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y)
                   1
                   0.001)
            1000)
;; 2.716923932235896
