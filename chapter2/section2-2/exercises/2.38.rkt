#lang racket

(require
  "2.37.rkt"
  rackunit)

(provide
 fold-left
 fold-right)

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(check-equal? 3/2 (fold-right / 1 (list 1 2 3)))
(check-equal? 1/6 (fold-left / 1 (list 1 2 3)))
(check-equal? '(1 (2 (3 ()))) (fold-right list null (list 1 2 3)))
(check-equal? '(((() 1) 2) 3) (fold-left list null (list 1 2 3)))

(check-equal? 3/8 (fold-right / 1 (list 1 2 3 4)))
(check-equal? 1/24 (fold-left / 1 (list 1 2 3 4)))

; If op is commutative, both fold methods will
; return the same result
; NOTE: Flawed reasoning, see below.

(check-equal?
 (fold-right + 0 '(1 2 3 4))
 (fold-left + 0 '(1 2 3 4)))

(check-equal?
 (fold-right * 1 '(1 2 3 4))
 (fold-left * 1 '(1 2 3 4)))

; After reading this solution,
; https://github.com/sarabander/p2pu-sicp/blob/master/2.2/2.38.scm
; I realised that both + and * operations are also associative, so I
; should have chosen an associative but not commutative operation to
; see which one was required. The book was probably giving us a clue
; by placing this exercise right after the matrix multiplication one,
; given that matrix multiplication is an excellent example of an
; associative but not commutative operation.

(define matrix1
  (cons-matrix
   (cons-vector 1 2 3)
   (cons-vector 1 0 1)
   (cons-vector 0 2 1)))

(define matrix2
  (cons-matrix
   (cons-vector -2 4 3)
   (cons-vector  1 2 1)
   (cons-vector  1 3 2)))

(define matrix3
  (cons-matrix
   (cons-vector  1  0  3)
   (cons-vector  1  2 -3)
   (cons-vector -3 -1  1)))

; Validating associative property
(check-equal?
 (matrix-*-matrix matrix1 (matrix-*-matrix matrix2 matrix3))
 (matrix-*-matrix (matrix-*-matrix matrix1 matrix2) matrix3))

; Asserting non-commutative
(check-not-equal?
 (matrix-*-matrix matrix1 matrix2)
 (matrix-*-matrix matrix2 matrix1))

; Validating equivalence for accumulation
(check-not-equal?
 (fold-right matrix-*-matrix matrix1 (list matrix2 matrix3))
 (fold-left matrix-*-matrix matrix1 (list matrix2 matrix3)))

; Contrary to the linked solution, I've found that associative
; operations are no good, and confirmed that the good property
; remains commutativity.
