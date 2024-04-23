#lang racket

(provide make-serializer)


(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (mcons false null)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire)  ; retry
               'done))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-mcar! cell false))
(define (test-and-set! cell)
  (if (mcar cell) true (begin (set-mcar! cell true) false)))
