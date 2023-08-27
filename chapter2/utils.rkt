#lang racket

(provide echo time-procedure)


(define (echo x)
    (display x)
    (newline)
    x)

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
