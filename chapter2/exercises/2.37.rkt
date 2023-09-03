; This exercise was a good opportunity to practice
; with abstractions and abstraction barriers.
; For that reason, you'll find work that wasn't
; asked for directly.

#lang racket

(require "../utils.rkt"
         "2.36.rkt"
         rackunit)


; Vector abstraction
; ======================
; Constructors
; ----------------------
(define (cons-vector . entries)
    entries)

(define (list->vec list_)
    list_)
; ----------------------

; Selectors
; ----------------------
(define (length-vector vec)
    (length vec))

(define (nth-elem-vector vec n)
    (cond ((null? vec) null)
          ((= n 0) (car vec))
          (else (nth-elem-vector (cdr vec) (- n 1)))))

(define (enumerate-vector vec) vec)
; ----------------------
; ======================

(check-equal? 7 (nth-elem-vector (cons-vector 1 4 7) 2))
(check-equal? 1 (nth-elem-vector (cons-vector 1 4 7) 0))
(check-equal? null (nth-elem-vector (cons-vector 1 4 7) 3))

(define (all-equal-length? vectors)
    (let ((ref (length-vector (car vectors))))
        (accumulate
            (lambda (elem accum) (and elem accum))
            true
            (map (lambda (vec) (= (length-vector vec) ref)) (cdr vectors)))))

(check-true
    (all-equal-length? (list (cons-vector 1 0 0)
                             (cons-vector 0 1 0)
                             (cons-vector 0 0 1))))
(check-false
    (all-equal-length? (list (cons-vector 1 0 0)
                             (cons-vector 0 1)
                             (cons-vector 0 0 1))))

; Matrix abstraction
; ======================
; Constructors
; ----------------------
(define (cons-matrix . vectors)
    (if (not (all-equal-length? vectors))
        (raise-user-error "Vectors should be of the same length")
        vectors))

(define (list->matrix list_)
    (if (not (all-equal-length? list_))
        (raise-user-error "Vectors should be of the same length")
        list_))

(define (cols->matrix list-of-columns)
    (if (null? (car list-of-columns))
        null
        (cons
            (map car list-of-columns)
            (cols->matrix (map cdr list-of-columns)))))
; ----------------------

; Selectors
; ----------------------
(define (enum-matrix-rows matrix)
    matrix)

(define (enum-matrix-cols matrix)
    (if (null? (car matrix))
        null
        (cons (map car matrix) (enum-matrix-cols (map cdr matrix)))))
; ----------------------

(define (dot-product v w)
    (if (not (all-equal-length? (list v w)))
        (raise-user-error "Vectors should be of the same length")
        (accumulate
            +
            0
            (map * (enumerate-vector v) (enumerate-vector w)))))

(define (matrix-*-vector m v)
    (list->vec (map (lambda (row) (dot-product row v)) m)))

(define (vector-*-matrix v m)
    (list->matrix
        (map
            (lambda (col) (dot-product v col))
            (enum-matrix-cols m))))

(define (matrix-*-matrix m n)
    (cols->matrix
        (map
            (lambda (col) (matrix-*-vector m col))
            (enum-matrix-cols n))))


(define vec1 (cons-vector 1 2 3))
(define vec2 (cons-vector 1 0 -1))
(define vec3 (cons-vector 1 1 1))
(define vec4 (cons-vector 1 -2 1))
(define vec5 (cons-vector 0 -1 1))

(check-equal? -2 (dot-product vec1 vec2))
(check-equal? 0 (dot-product vec2 vec3))

(define matrix1 (cons-matrix vec1 vec2 vec3))
(define matrix2 (cons-matrix vec3 vec4))

(check-equal?
    '((1 1 1) (2 0 1) (3 -1 1))
    (enum-matrix-cols matrix1))

(check-equal?
    (cons-vector 0 0 0)
    (matrix-*-vector matrix1 vec4))

(check-equal?
    (cons-vector 0 3)
    (matrix-*-vector matrix2 vec5))

(check-equal?
    (cons-matrix
        (cons-vector 6 5 4)
        (cons-vector 0 1 2)
        (cons-vector 3 3 3))
    (matrix-*-matrix matrix1 matrix1))
