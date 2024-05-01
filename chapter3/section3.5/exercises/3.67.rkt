#lang racket

;; Modify the pairs procedure so that (pairs integers integers) will produce
;; the stream of all pairs of integers (i, j) (without the condition i ≤ j).
;; Hint: You will need to mix in an additional stream.

(require rackunit)

;; S1,T1 | S1,T2   S1,T3   S1,T4   S1,T5   ...
;; ------|------------------------------------
;; S2,T1 | S2,T2 | S2,T3   S2,T4   S2,T5   ...
;;       |-------|----------------------------
;; S3,T1 | S3,T2 | S3,T3 | S3,T4   S3,T5   ...
;;       |       |-------|--------------------
;; S4,T1 | S4,T2 | S4,T3 | S4,T4 | S4,T5   ...

(define integers (stream-cons 1 (stream-map add1 integers)))

(define (interleave . streams)
  (cond [(empty? streams) empty-stream]
        [(stream-empty? (car streams))
         (apply interleave (cdr streams))]
        [else
          (stream-cons
            (stream-first (car streams))
            (apply interleave
                   (append
                     (cdr streams)
                     (list (stream-rest (car streams))))))]))

(check-equal? (stream->list
                (stream-take (interleave integers integers integers) 12))
              '(1 1 1 2 2 2 3 3 3 4 4 4))

(define (pairs s t)
  (let ([sf (stream-first s)]
        [sr (stream-rest s)]
        [tf (stream-first t)]
        [tr (stream-rest t)])
  (stream-cons
    (cons sf tf)
    (interleave
      (stream-map (lambda (x) (cons x tf)) sr)
      (stream-map (lambda (x) (cons sf x)) tr)
      (pairs sr tr)))))

(check-equal? (stream->list
                (stream-take (pairs integers integers) 7))
              '((1 . 1) (2 . 1) (1 . 2) (2 . 2) (3 . 1) (1 . 3) (3 . 2))) 
