#lang racket

(require rackunit)

(define (count-pairs root)
  (length (unique-pairs root)))

(define (unique-pairs root)
  (if (not (pair? root))
    (empty-memo)
    (add-to-memo 
      (combine-memos (unique-pairs (car root))
                     (unique-pairs (cdr root)))
      root)))

(define (combine-memos m1 m2)
  (if (empty? m1)
    m2
    (combine-memos (cdr m1)
                   (add-to-memo m2 (car m1)))))

(define (empty-memo) '())

(define (in-memo? memo elem)
  (not (eq? (memq elem memo) false)))

(define (add-to-memo memo elem)
  (if (in-memo? memo elem)
    memo
    (cons elem memo)))

(check-eq? (combine-memos (empty-memo) (empty-memo)) (empty-memo))
(check-equal? (combine-memos (empty-memo) '(1)) '(1))
(check-equal? (add-to-memo (empty-memo) 1) '(1))
(check-equal? (add-to-memo '(1) 1) '(1))
(check-false (in-memo? (empty-memo) 1))
(check-true (in-memo? '(1) 1))
(check-false (in-memo? '(1) 2))
(check-true (in-memo? '(1 2) 2))

(define x 'a)
(check-eq? (count-pairs (cons x (cons x (cons x x)))) 3)

(define c (cons x x))
(define b (cons c c))
(define a (cons x b))
(check-eq? (count-pairs a) 3)

(define f (cons x x))
(define e (cons f f))
(define d (cons e e))
(check-eq? (count-pairs d) 3)


(define (count-mpairs root)
  (length (unique-mpairs root)))

(define (unique-mpairs root)
  (if (not (mpair? root))
    (empty-memo)
    (add-to-memo 
      (combine-memos (unique-pairs (mcar root))
                     (unique-pairs (mcdr root)))
      root)))
(define i (mcons x x))
(define h (mcons x i))
(define g (mcons x h))
(set-mcdr! i g)
(check-eq? (count-mpairs g) 3 "Should not loop forever")
;; My functional implementation is not correct, but it does not loop forever

(define (count-unique-mpairs root)
  (let ((memo '()))
      (define (loop elem)
        (if (or (not (mpair? elem))
                (memq elem memo))
          0
          (begin
            (set! memo (cons elem memo))
            (+ 1
               (loop (mcar elem))
               (loop (mcdr elem))))))
      (loop root)))

(check-eq? (count-unique-mpairs g) 3 "Should not loop forever")
