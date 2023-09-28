#lang racket

(require "2.63.rkt"
         rackunit)

;; From the book
;; Lookup by key, O(n), unordered list representation
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;; Auxiliary definitions
(define (make-record key value)
  (cons key value))

(define (key record)
  (car record))

;; Solution
(define (lookup-tree given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((this-key (key (entry set-of-records))))
        (cond
          ((equal? given-key this-key)
           (entry set-of-records))
          ((< this-key given-key)
           (lookup-tree given-key (right-branch set-of-records)))
          ((> this-key given-key)
           (lookup-tree given-key (left-branch set-of-records)))))))

(define dataset
  (make-tree (make-record 7 "A")
             (make-tree (make-record 3 "B")
                        (make-leave (make-record 1 "C"))
                        (make-leave (make-record 4 "D")))
             (make-tree (make-record 8 "E")
                        '()
                        (make-leave (make-record 9 "F")))))

(check-equal? (lookup-tree 4 dataset) (make-record 4 "D"))
(check-equal? (lookup-tree 8 dataset) (make-record 8 "E"))
(check-equal? (lookup-tree 9 dataset) (make-record 9 "F"))
(check-false (lookup-tree 5 dataset))
