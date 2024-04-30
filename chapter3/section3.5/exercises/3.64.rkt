#lang racket

(require
  "../../modules/streams.rkt"
  rackunit)

;; Write a procedure stream-limit that takes as arguments a stream and a
;; number (the tolerance). It should examine the stream until it ﬁnds two
;; successive elements that diﬀer in absolute value by less than the tolerance,
;; and return the second of the two elements. Using this, we could compute
;; square roots up to a given tolerance by

(define (average . nums)
  (/ (foldl + 0 nums) (length nums)))

(define (sqrt-improve guess x)
  (exact->inexact (average guess (/ x guess))))

(define (sqrt-stream x)
  (define guesses
    (stream-cons
      1
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)

(define (sqrt-from-stream x tolerance)
  (stream-limit-v2 (sqrt-stream x) tolerance))

(define (zip-streams s1 s2)
  (stream-cons (cons (stream-first s1) (stream-first s2))
               (zip-streams (stream-rest s1) (stream-rest s2))))

(define (stream-limit s tolerance)
  (cond [(stream-empty? s)
    (error "Stream doesn't contain a limiting sequence")]
        [(< (abs (- (stream-first s) (stream-ref s 1))) tolerance)
         (stream-ref s 1)]
        [else (stream-limit (stream-rest s) tolerance)]))

(define (stream-find pred s)
  (stream-first (stream-filter pred s)))

(define (stream-limit-v2 s tolerance)
  ;; original effort was simpler and, as far as I can see, at least
  ;; as performant. Just trying this out
  (let ([zipped (zip-streams (stream-rest s) s)])
    (car (stream-find (lambda (elem)
                        (< (abs (- (car elem) (cdr elem))) tolerance))
                      zipped))))

(check-within (sqrt-from-stream 10 .001) (sqrt 10) .001)
