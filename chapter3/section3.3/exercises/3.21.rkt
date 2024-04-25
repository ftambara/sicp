#lang racket

(require compatibility/mlist)

(provide
  queue-pop!
  queue-push!
  make-queue
  see-front)

(define (queue-pop! q)
  (if (empty-queue? q)
    (error "Cannot pop from empty queue")
    (let ((old-front (front-ptr q)))
      (set-front-ptr! q (mcdr (front-ptr q)))
      (mcar old-front))))

(define (see-front q)
  (front-ptr q))

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

;; Exercise
(define q1 (make-queue))
(queue-push! q1 'a)
;; (mcons (mcons 'a '()) (mcons 'a '()))
;; Front pointer: (a)
;; Rear pointer: (a)
(queue-push! q1 'b)
;; (mcons (mcons 'a (mcons 'b '())) (mcons 'b '()))
;; Front pointer: (a b)
;; Rear pointer: (b)
(queue-pop! q1)
;; 'a
q1
;; (mcons (mcons 'b '()) (mcons 'b '()))
;; Front pointer: (b)
;; Rear pointer: (b)
;; Underlying list: (a b)
(queue-pop! q1)
;; 'b
q1
;; (mcons '() (mcons 'b '()))
;; Front pointer: ()
;; Rear pointer: (b)
;; Rear pointer is never 'cleaned up', and the underlying list
;; still exists.
;; A custom print-queue procedue should know when a queue is empty by
;; looking at the front pointer, and act accordingly

(define (print-queue q)
  (display "Queue(")
  (if (not (empty-queue? q))
    (printf "front: ~v, rear: ~v"
            (mcar (front-ptr q))
            (mcar (rear-ptr q)))
    void)
  (display ")\n"))

(print-queue q1)

(define q2 (make-queue))
(queue-push! q2 1)
(print-queue q2)
(queue-push! q2 2)
(print-queue q2)
