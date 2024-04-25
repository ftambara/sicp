#lang racket

(require
  compatibility/mlist
  rackunit)

(define (make-queue)
  (let ((front-ptr (mlist))
        (rear-ptr (mlist)))
    (define (pop!)
      (if (null? front-ptr)
        null
        (begin
          (let ((popped (mcar front-ptr)))
            (set! front-ptr (mcdr front-ptr))
            popped))))

    (define (push! val)
      (let ((new-pair (mcons val null)))
        (if (null? front-ptr)
          (set! front-ptr new-pair)
          (set-mcdr! rear-ptr new-pair))
        (set! rear-ptr new-pair)
        'ok))

    (define (queue-empty?) (null? front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'pop!) pop!)
            ((eq? m 'push!) push!)
            ((eq? m 'empty?) queue-empty?)
            ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            (else (error "Invalid message" m))))
    dispatch))

(define (print-queue q)
  (display "Queue(")
  (if (not ((q 'empty?)))
    (printf "front: ~v, rear: ~v"
            (mcar (q 'front-ptr))
            (mcar (q 'rear-ptr)))
    void)
  (display ")\n"))

(define q1 (make-queue))
((q1 'push!) 'a)
((q1 'push!) 'b)
(print-queue q1)
(check-eq? ((q1 'pop!)) 'a)
(check-false ((q1 'empty?)))
(print-queue q1)
(check-eq? ((q1 'pop!)) 'b)
(check-true ((q1 'empty?)))
(print-queue q1)

(define q2 (make-queue))
((q2 'push!) 1)
(print-queue q2)
((q2 'push!) 2)
(print-queue q2)
