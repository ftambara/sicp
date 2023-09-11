#lang racket

(require rackunit)


(define (tree-map proc tree)
  (map (lambda (node)
         (if (pair? node)
             (tree-map proc node)
             (proc node)))
       tree))

(define (square x)
  (* x x))

(define (square-tree tree) (tree-map square tree))

(check-equal?
 (square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
 '(1 (4 (9 16) 25) (36 49)))
