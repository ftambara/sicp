#lang racket

;; Use the series
;; ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
;; to compute three sequences of approximations to the natural logarithm of 2,
;; in the same way we did above for π. How rapidly do these sequences converge?

(require
  "../../modules/streams.rkt"
  rackunit)

(define (ln2-summands n)
  (stream-cons
    (/ n)
    (let ([incr (if (negative? n) -1 1)])
      (ln2-summands (- (+ n incr))))))

(check-equal? (stream->list (stream-take (ln2-summands 1) 4))
              '(1 -1/2 1/3 -1/4))

(define (partial-sums s)
   (add-streams s (stream-cons 0 (partial-sums s))))

(define (euler-transform s)
  (let ([s0 (stream-ref s 0)]   ; S_(n-1)
        [s1 (stream-ref s 1)]   ; S_n
        [s2 (stream-ref s 2)])  ; S_(n+1)
    (stream-cons (- s2 (/ (expt (- s2 s1) 2)
                          (+ s0 (- (* 2 s1)) s2)))
                 (euler-transform (stream-rest s)))))

(define (accelerated-sequence transform s)
    (define (make-tableau transform s)
      (stream-cons s (make-tableau transform (transform s))))
  (stream-map stream-first (make-tableau transform s)))

(define ln2-stream
  (partial-sums (stream-map exact->inexact (ln2-summands 1))))

(log 2)
;; 0.6931471805599453

(stream-display (stream-take ln2-stream 8))
;; 1.0
;; 0.5
;; 0.8333333333333333
;; 0.5833333333333333
;; 0.7833333333333332
;; 0.6166666666666666
;; 0.7595238095238095
;; 0.6345238095238095

(stream-display (stream-take (euler-transform ln2-stream) 8))
;; 0.7
;; 0.6904761904761905
;; 0.6944444444444444
;; 0.6924242424242424
;; 0.6935897435897436
;; 0.6928571428571428
;; 0.6933473389355742
;; 0.6930033416875522
;;   ^^^
;; Accurate to 3 decicmal places in eight iterations

(stream-display (stream-take (accelerated-sequence euler-transform ln2-stream) 8))
;; 1.0
;; 0.7
;; 0.6932773109243697
;; 0.6931488693329254
;; 0.6931471960735491
;; 0.6931471806635636
;; 0.6931471805604039
;; 0.6931471805599445
;;   ^^^^^^^^^^^^^^
;; Accurate to 14 decimal places in eight iterations
