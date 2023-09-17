#lang racket

(require
  "../../utils.rkt"
  rackunit)

(provide
 enumerate-interval
 flatmap)


(define (enumerate-interval start end)
  (if (> start end)
      null
      (cons start (enumerate-interval (+ start 1) end))))

(check-equal? (enumerate-interval 2 5) '(2 3 4 5))

(define (flatmap proc sequence)
  (accumulate append null (map proc sequence)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(check-equal?
 (unique-pairs 4)
 '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (let ((first (car pair))
        (second (cadr pair)))
    (list first second (+ first second))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(check-equal?
 (prime-sum-pairs 5)
 '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7)))
