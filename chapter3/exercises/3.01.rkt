#lang racket

(require rackunit)

(define (make-accumulator sum)
  (lambda (num)
    (begin
      (set! sum (+ sum num))
      sum)))

(let ((acc (make-accumulator 0)))
  (check-eq? (acc 5) 5)
  (check-eq? (acc 5) 10)
  (check-eq? (acc -2) 8)
  (let ((acc2 (make-accumulator 0)))
      (check-eq? (acc2 1) 1)
      (check-eq? (acc 1) 9)))
