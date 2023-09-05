#lang racket

(provide accumulate
         echo
         prime?
         time-procedure)


; From the book
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (echo x)
    (display x)
    (newline)
    x)

(define (prime? number)
    (define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)
                (remainder (square (expmod base (/ exp 2) m)) m))
              (else
                (remainder (* base (expmod base (- exp 1) m)) m))))
    (define (fermat-test n times)
        (define (try-it a)
            (= (expmod a n n) a))
        (or (= times 0)
            (and (try-it (+ 1 (random (- n 1))))
                 (fermat-test n (- times 1)))))
    (fermat-test number (max number 100)))

(define (time-procedure proc n)
    ; Show average and standard deviation
    ; of running proc for n number of repetitions
    (define (average items)
        (/ (apply + items) (length items)))
    (define (square x) (* x x))
    (define (stddev items)
        (sqrt (average (map square (map - items (build-list (length items) (lambda (x) (average items))))))))
    (define (time-1 proc)
        (let ((start (current-milliseconds)))
            (proc)
            (- (current-milliseconds) start)))
    (let ((times (map time-1 (build-list n (lambda (x) proc)))))
        (printf "Average: ~a ms\n" (exact->inexact (average times)))
        (printf "Standard deviation: ~a ms\n" (stddev times))))
