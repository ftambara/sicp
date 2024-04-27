#lang racket

(provide
  stream-car
  stream-cdr
  stream-null?
  the-empty-stream
  stream-display
  stream-map-book
  stream-filter-book
  stream-ref-book
  stream-enumerate-interval)


(define stream-car stream-first)
(define stream-cdr stream-rest)
(define stream-null? stream-empty?)
(define the-empty-stream empty-stream)

(define (stream-ref-book stream n)
  (cond [(< n 0) (error "Argument n cannot be negative")]
        [(= n 0) (stream-car stream)]
        [else (stream-ref-book (stream-cdr stream) (sub1 n))]))

(define (stream-map-book proc stream)
  (if (stream-null? stream)
    the-empty-stream
    (stream-cons #:eager (proc (stream-car stream))
                 (stream-map-book proc (stream-cdr stream)))))

(define (stream-filter-book pred stream)
  (if (stream-null? stream)
    the-empty-stream
    (if (pred (stream-car stream))
      (stream-cons #:eager (stream-car stream)
                   (stream-filter-book pred (stream-cdr stream)))
      (stream-filter-book pred (stream-cdr stream)))))

(define (stream-for-each-book proc stream)
  (if (stream-null? stream)
    'done
    (begin
      (proc (stream-car stream))
      (stream-for-each-book proc (stream-cdr stream)))))

(define (stream-display stream)
  (display "Stream: ")
  (stream-for-each-book (lambda (elem) (displayln elem))
                   stream))

(define (stream-enumerate-interval start end)
  (if (> start end)
    empty-stream
    (stream-cons #:eager start
                 (stream-enumerate-interval (add1 start) end))))
