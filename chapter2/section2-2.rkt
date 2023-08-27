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
