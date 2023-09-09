#lang racket

(require
    "2.46.rkt"
    rackunit)

(provide
    make-frame
    origin-frame
    edge1-frame
    edge2-frame)


(define (make-frame1 origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame1 frame)
    (car frame))

(define (edge1-frame1 frame)
    (cadr frame))

(define (edge2-frame1 frame)
    (caddr frame))

(define (make-frame2 origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
    (car frame))

(define (edge1-frame2 frame)
    (cadr frame))

(define (edge2-frame2 frame)
    (cddr frame))

;; Tests
(define orig (make-vect 1 2))
(define e1 (make-vect 3 4))
(define e2 (make-vect 5 6))

(define frm1 (make-frame1 orig e1 e2))
(define frm2 (make-frame2 orig e1 e2))

(check-equal? (origin-frame1 frm1) orig)
(check-equal? (edge1-frame1 frm1) e1)
(check-equal? (edge2-frame1 frm1) e2)

(check-equal? (origin-frame1 frm1) (origin-frame2 frm2))
(check-equal? (edge1-frame1 frm1) (edge1-frame2 frm2))
(check-equal? (edge2-frame1 frm1) (edge2-frame2 frm2))


;; Exports
(define make-frame make-frame1)
(define origin-frame origin-frame1)
(define edge1-frame edge1-frame1)
(define edge2-frame edge2-frame1)
