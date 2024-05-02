#lang racket

(provide
  stream-car
  stream-cdr
  stream-null?
  the-empty-stream
  stream-display
  stream-map-book
  stream-map-all
  stream-filter-book
  stream-ref-book
  stream-enumerate-interval
  add-streams
  mult-streams
  scale-stream
  pairs
  interleave
  merge
  pairs-weighted
  integers)


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

(define (stream-map-all proc . argstreams)
  (if (stream-empty? (car argstreams))
    empty-stream
    (stream-cons
      (apply proc (map stream-first argstreams))
      (apply stream-map-all proc (map stream-rest argstreams)))))

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
  (stream-for-each-book (lambda (elem) (displayln elem))
                   stream))

(define (stream-enumerate-interval start end)
  (if (> start end)
    empty-stream
    (stream-cons #:eager start
                 (stream-enumerate-interval (add1 start) end))))

(define (add-streams s1 s2)
  (stream-map-all + s1 s2))

(define (mult-streams s1 s2)
  (stream-map-all * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* factor x)) stream))

(define (pairs s t)
  (let ([s0 (stream-first s)])
    (stream-cons
      (list s0 (stream-first t))
      (interleave
        (stream-map (lambda (ti) (list s0 ti)) (stream-rest t))
        (pairs (stream-rest s) (stream-rest t))))))

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

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (stream-cons
                     s1car
                     (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (stream-cons
                     s2car
                     (merge s1 (stream-cdr s2))))
                  (else
                    (stream-cons
                      s1car
                      (merge (stream-cdr s1)
                             (stream-cdr s2)))))))))

(define (merge-weighted pairs1 pairs2 weight)
  (cond [(stream-empty? pairs1) pairs2]
        [(stream-empty? pairs2) pairs1]
        [else
          (let ([p1 (stream-first pairs1)]
                [p1cdr (stream-rest pairs1)]
                [p2 (stream-first pairs2)]
                [p2cdr (stream-rest pairs2)])
            (if (< (weight p1) (weight p2))
              (stream-cons p1 (merge-weighted p1cdr pairs2 weight))
              (stream-cons p2 (merge-weighted pairs1 p2cdr weight))))]))

(define (pairs-weighted s t weight)
  (stream-cons
    (list (stream-first s) (stream-first t))
    (merge-weighted
      (stream-map (lambda (ti) (list (stream-first s) ti)) (stream-rest t))
      (pairs-weighted (stream-rest s) (stream-rest t) weight)
      weight)))

(define integers (stream-cons 1 (stream-map add1 integers)))
