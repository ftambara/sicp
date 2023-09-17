#lang racket

(provide reverse-list reverse-list-app time-procedure)

(require "../../utils.rkt")

(define (reverse-list list_)
  (define (iter reversed remain)
    (if (null? remain)
        reversed
        (iter (cons (car remain) reversed) (cdr remain))))
  (iter '() list_))

; reverse-list could also be defined with the help of
; append, but that would make the procedure O(n^2) for time
; this implementation is O(n) in both time and memory.
(define (reverse-list-app list_)
  (if (null? list_)
      list_
      (append (reverse-list-app (cdr list_)) (list (car list_)))))

; (define list4 (build-list 10000 values))
; (define list5 (build-list 100000 values))
; (define list6 (build-list 1000000 values))

; (display "reverse-list\n")
; (time-procedure (lambda () (reverse-list list4)) 10)
; (time-procedure (lambda () (reverse-list list5)) 10)
; (time-procedure (lambda () (reverse-list list6)) 10)

; (newline)
; (display "reverse-list-app\n")
; (time-procedure (lambda () (reverse-list-app list4)) 10)
; (time-procedure (lambda () (reverse-list-app list5)) 1)
;   Run only once, commented out, takes too long
; (time-procedure (lambda () (reverse-list-app list6)) 10)
;   Don't run, would take hours

; Results:
; reverse-list
; Average: 0.3 ms
; Standard deviation: 0.45... ms
; Average: 4.2 ms
; Standard deviation: 2.71... ms
; Average: 48.7 ms
; Standard deviation: 7.23... ms

; reverse-list-app
; Average: 223.9 ms
; Standard deviation: 6.22... ms
; Average: 44837.0 ms
; Standard deviation: 0 ms

; Both sets of measurments agree with what it was theorized.
