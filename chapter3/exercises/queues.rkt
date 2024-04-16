#lang racket

(require compatibility/mlist)

(provide
  queue-pop!
  queue-push!
  make-queue
  see-front
  empty-queue?
  print-queue)

(define (queue-pop! q)
  (if (empty-queue? q)
    (error "Cannot pop from empty queue")
    (let ((old-front (front-ptr q)))
      (set-front-ptr! q (mcdr (front-ptr q)))
      (mcar old-front))))

(define (see-front q)
  (mcar (front-ptr q)))

(define (queue-push! q item)
  (let ((new-pair (mcons item null)))
    (if (empty-queue? q)
      (begin
        (set-front-ptr! q new-pair)
        (set-rear-ptr! q new-pair)
        q)
      (begin
        (set-mcdr! (rear-ptr q) new-pair)
        (set-rear-ptr! q new-pair)
        q))))

(define (front-ptr q) (mcar q))

(define (rear-ptr q) (mcdr q))

(define (set-front-ptr! q pair)
  (set-mcar! q pair))

(define (set-rear-ptr! q pair)
  (set-mcdr! q pair))

(define (make-queue)
  ;; Both underlying lists need to be mutable
  (mcons (mlist) (mlist)))

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (print-queue q)
  (display "Queue(")
  (if (not (empty-queue? q))
    (printf "front: ~v, rear: ~v"
            (mcar (front-ptr q))
            (mcar (rear-ptr q)))
    void)
  (display ")\n"))
