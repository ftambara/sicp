#lang racket

;; In a similar way to Exercise 3.71 generate a stream of all numbers that can
;; be written as the sum of two squares in three different ways (showing how
;; they can be so written).

(require
  "../../modules/streams.rkt"
  rackunit)

(define (take-at-most stream n)
  (if (or (stream-empty? stream) (= n 0))
    empty-stream
    (stream-cons (stream-first stream)
                 (take-at-most (stream-rest stream) (sub1 n)))))

(check-equal? (stream->list (take-at-most (stream 1 2) 5)) '(1 2))
(check-equal? (stream->list (take-at-most (stream 1 2) 2)) '(1 2))
(check-equal? (stream->list (take-at-most (stream 1 2) 1)) '(1))
(check-equal? (stream->list (take-at-most (stream 1 2) 0)) '())

(define (any? pred elems)
  (if (empty? (cdr elems))
    (pred (car elems))
    (or (pred (car elems))
        (any? pred (cdr elems)))))

(define (all? pred elems)
  (if (empty? (cdr elems))
    (pred (car elems))
    (and (pred (car elems))
         (all? pred (cdr elems)))))

(check-true (any? even? '(3 5 4 9)))
(check-false (all? even? '(3 5 4 9)))
(check-true (all? even? '(4 6 4 10)))

(define (stream-take-dups stream refweight weight)
  (if (stream-empty? stream)
    empty-stream
    (if (= refweight (weight (stream-first stream)))
      (stream-cons (stream-first stream)
                   (stream-take-dups (stream-rest stream) refweight weight))
      empty-stream)))

(define (n-duplicates stream n weight)
  (if (< (length (stream->list (take-at-most stream n))) n)
    empty-stream
    (let* ([w1 (weight (stream-first stream))]
           [dup-pairs (stream-take-dups stream w1 weight)]
           [n-dups (length (stream->list dup-pairs))])
      (if (>= n-dups n)
        (stream-cons (stream->list dup-pairs)
                     (n-duplicates (stream-tail stream n-dups) n weight))
        (n-duplicates (stream-tail stream n-dups) n weight)))))

(define (sum-of-squares p)
  (+ (* (car p) (car p)) (* (cadr p) (cadr p))))

(check-equal?
  (stream->list
    (stream-take-dups
      (stream '(1 2) '(2 1) '(3 4))
      5
      sum-of-squares))
  '((1 2) (2 1)))

(check-equal?
  (stream->list
    (stream-take-dups
      (stream '(1 1) '(1 2) '(2 1))
      5
      sum-of-squares))
  '())

(displayln "Numbers that can be written as the sum of two squares in at least
           three different ways:")

(define (print-result pairs)
  (display (sum-of-squares (car pairs)))
  (display " = ")
  (displayln
    (string-join
      (map
        (lambda (pair)
          (string-join
            (map (lambda (x) (string-append (number->string x) "^2"))
                 pair)
            " + "))
        pairs)
      " = ")))

(stream-for-each print-result
  (stream-take
    (n-duplicates (pairs-weighted integers integers sum-of-squares) 3 sum-of-squares)
    10))
