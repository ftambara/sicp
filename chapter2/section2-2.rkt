#lang racket

(define (nth-list list_ pos)
    (cond ((= pos 0) (car list_))
          ((or (< pos 0) (null? (cdr list_))) (error "Out of bounds"))
          (else (nth-list (cdr list_) (- pos 1)))))

(define (append-lists list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append-lists (cdr list1) list2))))

(require "exercises/2.17.rkt")
(require "exercises/2.18.rkt")
(require "exercises/2.19.rkt")
(require "exercises/2.20.rkt")


(define (map-list func list_)
    (if (null? list_)
        list_
        (cons (func (car list_)) (map-list (cdr list_) func))))

(define (scale-list list_ factor)
    (map-list (lambda (x) (* factor x)) list_))


(require "exercises/2.21.rkt")
(require "exercises/2.22.rkt")
(require "exercises/2.23.rkt")


(define (length-list list_)
    (define (iter count remain)
        (if (null? remain)
            count
            (iter (+ count 1) (cdr remain))))
    (iter 0 list_))

(define (count-leaves tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) 1)
          (else
            (+ (count-leaves (car tree))
               (count-leaves (cdr tree))))))

(define (scale-tree tree factor)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (scale-tree sub-tree factor)
                (* factor sub-tree)))
         tree))

(define (filter predicate sequence)
    (cond ((null? sequence) sequence)
          ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(filter odd? '(1 2 3 5 90 99))
; => (1 3 5 99)

; (define (accumulate op initial sequence)
;     (if (null? sequence)
;         initial
;         (accumulate op (op (car sequence) initial) (cdr sequence))))
; This implementation I thought of doesn't work for an op like cons,
; since it reverses the intended result

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))


(accumulate + 0 (list 1 2 3 4 5))
; => 15
(accumulate * 1 (list 1 2 3 4 5))
; => 120
(accumulate cons null (list 1 2 3 4 5))
; => (1 2 3 4 5)

(define (enumerate-interval low high)
    (if (> low high)
        null
        (cons low (enumerate-interval (add1 low) high))))

(enumerate-interval 2 7)
; (2 3 4 5 6 7)

(define (enumerate-tree tree)
    (cond ((null? tree) tree)
          ((list? tree)
            (append (enumerate-tree (car tree))
                    (enumerate-tree (cdr tree))))
          (else (list tree))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; (1 2 3 4 5)

(define (square x)
    (* x x))

(define (fib n)
    (define (iter two_ago one_ago count)
        (if (= count n)
            (+ two_ago one_ago)
            (iter one_ago (+ two_ago one_ago) (+ count 1))))
    (iter 1 0 0))

(define (sum-odd-squares tree)
    (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
    ; Why use an identity operation like this accumulate?
    ; Is the consistency worth it?
    (accumulate
        cons '() (filter even? (map fib (enumerate-interval 0 n)))))
