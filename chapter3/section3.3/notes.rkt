#lang racket

(require compatibility/mlist)

;; “The desire to model systems composed of objects that have changing state
;; leads us to the need to modify compound data objects, as well as to
;; construct and select from them.”
;; I don't see how we jumped to compound data objects from that argument.

;; Mutable here probably refers to something slightly different that what I'm
;; used to from other languages. In this context, any bound variable that
;; has a procedure that can change its value is mutable. This is different
;; from my understanding of mutable in other languages, where it refers to
;; types that allow their values to be changed. I wouldn't have considered
;; instance variables to be mutable, or merely reassigning a variable to a
;; new value to be mutation.
;; These two views can be reconciled by considering that in other languages,
;; variables are mutable, but the values they reference may not be (for
;; example, integer values are immutable, while array values are mutable, even
;; if we don't affect the variable's reference).

;; Identity is tested by simply comparing pointers. Apparently, this is
;; not sufficient for more complex programs (I can't see why, though).

;; Queues
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

(define (front-ptr q) (car q))

(define (rear-ptr q) (cdr q))

(define (set-front-ptr! q pair)
  (set-mcar! q pair))

(define (set-rear-ptr! q pair)
  (set-mcdr! q pair))

(define (make-queue)
  ;; Both underlying lists need to be mutable
  (cons (mlist) (mlist)))

(define (empty-queue? q)
  (null? (front-ptr q)))
