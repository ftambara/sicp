#lang racket

(require rackunit)


(define (square x)
  (* x x))

(define (square-tree tree)
  (let ((node tree))
    (cond ((null? node) node)
          ((pair? node)
           (cons (square-tree (car node))
                 (square-tree (cdr node))))
          (else (square node)))))

(define (square-tree-map tree)
  (map (lambda (node)
         (if (pair? node)
             (square-tree-map node)
             (square node)))
       tree))


(check-equal?
 (square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
 '(1 (4 (9 16) 25) (36 49)))

(check-equal?
 (square-tree-map
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
 '(1 (4 (9 16) 25) (36 49)))