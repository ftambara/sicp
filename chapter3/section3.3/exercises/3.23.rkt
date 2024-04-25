#lang racket

(require rackunit)

;; Double ended link
(define (make-delink val before after)
  (cons val (mcons before after)))
(define (delink-value delink) (car delink))
(define (delink-before delink) (mcar (cdr delink)))
(define (delink-set-before! delink new)
  (set-mcar! (cdr delink) new))
(define (delink-after delink) (mcdr (cdr delink)))
(define (delink-set-after! delink new)
  (set-mcdr! (cdr delink) new))

(define (set-front-ptr! q val)
  (set-mcar! q val))

(define (set-rear-ptr! q val)
  (set-mcdr! q val))

(define (make-deque)
  ;; A deque consists of a pair, pointing to the first and
  ;; last double ended links that form the queue, respectively
  (mcons null null))

(define (empty-deque? q)
  (and (null? (mcar q)) (null? (mcdr q))))

(define (front-deque q) (mcar q))

(define (rear-deque q) (mcdr q))

(define (front-insert-deque! q val)
  (if (empty-deque? q)
    (let ((new-delink (make-delink val null null)))
      (set-front-ptr! q new-delink)
      (set-rear-ptr! q new-delink))
    (let ((new-delink (make-delink val null (front-deque q))))
      (delink-set-before! (front-deque q) new-delink)
      (set-front-ptr! q new-delink)))
  'ok)

(define (rear-insert-deque! q val)
  (if (empty-deque? q)
    (let ((new-delink (make-delink val null null)))
      (set-front-ptr! q new-delink)
      (set-rear-ptr! q new-delink))
    (let ((new-delink (make-delink val (rear-deque q) null)))
      (delink-set-after! (rear-deque q) new-delink)
      (set-rear-ptr! q new-delink)))
  'ok)

(define (front-delete-deque! q)
  (cond ((empty-deque? q)
         (error "Cannot delete from empty deque"))
        ;; Update both ends if it's the last item
        ((null? (delink-after (front-deque q)))
         (set-front-ptr! q null)
         (set-rear-ptr! q null))
        (else
          (let ((new-front (delink-after (front-deque q))))
            (set-front-ptr! q new-front)
            (delink-set-before! new-front null))))
  'ok)

(define (rear-delete-deque! q)
  (cond ((empty-deque? q)
         (error "Cannot delete from empty deque"))
        ;; Update both ends if it's the last item
        ((null? (delink-after (front-deque q)))
         (set-front-ptr! q null)
         (set-rear-ptr! q null))
        (else
          (let ((new-rear (delink-before (rear-deque q))))
            (set-rear-ptr! q new-rear)
            (delink-set-after! new-rear null))))
  'ok)


;; Tests
(define (print-deque dq)
  (display "Deque: ")
  (let loop ((delink (front-deque dq)))
    (if (null? delink)
      (display "\n")
      (begin
        (display (delink-value delink))
        (display " ")
        (loop (delink-after delink))))))

(define dq (make-deque))
(print-deque dq)
(check-true (empty-deque? dq))

(front-insert-deque! dq 'a)
(print-deque dq)
(check-false (empty-deque? dq))
(check-eq? (delink-value (front-deque dq)) 'a)
(check-eq? (delink-value (rear-deque dq)) 'a)

(rear-insert-deque! dq 'b)
(print-deque dq)
(check-false (empty-deque? dq))
(check-eq? (delink-value (front-deque dq)) 'a)
(check-eq? (delink-value (rear-deque dq)) 'b)

(rear-insert-deque! dq 'c)
(print-deque dq)
(check-false (empty-deque? dq))
(check-eq? (delink-value (front-deque dq)) 'a)
(check-eq? (delink-value (rear-deque dq)) 'c)

(front-delete-deque! dq)
(print-deque dq)
(check-false (empty-deque? dq))
(check-eq? (delink-value (front-deque dq)) 'b)
(check-eq? (delink-value (rear-deque dq)) 'c)

(rear-delete-deque! dq)
(print-deque dq)
(check-false (empty-deque? dq))
(check-eq? (delink-value (front-deque dq)) 'b)
(check-eq? (delink-value (rear-deque dq)) 'b)
