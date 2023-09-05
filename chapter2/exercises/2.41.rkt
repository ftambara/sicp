#lang racket

(require
    "2.40.rkt"
    "../utils.rkt"
    rackunit)


(define (unique-triples n)
    (flatmap
        (lambda (i)
            (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval 1 (- j 1))))
                 (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

(check-equal? (unique-triples 4)
    '((3 2 1) (4 2 1) (4 3 1) (4 3 2)))

(define (triple-sum? sum)
    (lambda (triple) (= (accumulate + 0 triple) sum)))

(define (exact-sum-triples n sum)
    (filter (triple-sum? sum) (unique-triples n)))

