#lang racket

(require rackunit)

(provide
 make-vect
 xcor-vect
 ycor-vect
 add-vect
 sub-vect
 scale-vect)


(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (make-vect
   (+ (xcor-vect vect1) (xcor-vect vect2))
   (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect
   (- (xcor-vect vect1) (xcor-vect vect2))
   (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect scalar vect)
  (make-vect
   (* scalar (xcor-vect vect))
   (* scalar (ycor-vect vect))))


(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))
(define -v2 (make-vect -3 -4))

(check-equal? (xcor-vect v1) 1)
(check-equal? (ycor-vect v1) 2)
(check-equal? (xcor-vect v2) 3)
(check-equal? (ycor-vect v2) 4)

(check-equal? (xcor-vect (add-vect v1 v2)) 4)
(check-equal? (ycor-vect (add-vect v1 v2)) 6)

(check-equal? (xcor-vect (sub-vect v1 v2)) -2)
(check-equal? (ycor-vect (sub-vect v1 v2)) -2)

(check-equal? (xcor-vect (scale-vect 2 v1)) 2)
(check-equal? (ycor-vect (scale-vect 2 v1)) 4)

(check-equal? (xcor-vect (scale-vect 0.5 v2)) 1.5)
(check-equal? (ycor-vect (scale-vect 0.5 v2)) 2.0)

(check-equal? (xcor-vect (scale-vect -1 v2)) -3)
(check-equal? (ycor-vect (scale-vect -1 v2)) -4)

(check-equal? (scale-vect -1 v2) -v2)
