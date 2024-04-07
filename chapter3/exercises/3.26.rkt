#lang racket

(require
  compatibility/mlist
  rackunit)

;; To implement a table using binary trees, we replace the backbone list
;; with a binary tree. Each node in the tree is a list of four elements:
;; the node key, the left subtree, the right subtree, and the record
;; associated with the key.
;; Records may store any value, including another table.
;; Keys must be comparable using the comparison function cmp, which is
;; passed as an argument to make-table.
;; Function cmp takes two arguments, a and b, and returns -1 if a < b, 0 if
;; a = b, and 1 if a > b.

(define (make-tree cmp)
  (define (make-node key record)
    (mlist key (mlist) (mlist) record))
  (define (node-key node) (mcar node))
  (define (node-left node) (mcar (mcdr node)))
  (define (node-right node) (mcar (mcdr (mcdr node))))
  (define (node-record node) (mcar (mcdr (mcdr (mcdr node)))))
  (define (set-node-record! node record)
    (set-mcar! (mcdr (mcdr (mcdr node))) record))
  (define (set-node-left! node left)
    (set-mcar! (mcdr node) left))
  (define (set-node-right! node right)
    (set-mcar! (mcdr (mcdr node)) right))

  (let ((tree (mlist)))
    (define (lookup key)
      (let loop ((node tree))
        (cond
          ((null? node) false)
          ((< (cmp key (node-key node)) 0) (loop (node-left node)))
          ((> (cmp key (node-key node)) 0) (loop (node-right node)))
          (else (node-record node)))))

    (define (insert! key record)
      (let loop ((node tree))
        (cond
          ;; If the tree is empty, create a new node.
          ((null? tree) (set! tree (make-node key record)))
          ;; Otherwise, insert the key and record in the appropriate subtree.
          ((< (cmp key (node-key node)) 0)
           (if (null? (node-left node))
             (set-node-left! node (make-node key record))
             (loop (node-left node))))
          ((> (cmp key (node-key node)) 0)
           (if (null? (node-right node))
             (set-node-right! node (make-node key record))
             (loop (node-right node))))
          ;; If the key is already in the tree, update the record.
          (else (set-node-record! node record)))))

    (define (dispatch message . args)
      (cond
        ((eq? message 'lookup) (apply lookup args))
        ((eq? message 'insert!) (apply insert! args))
        (else (error "Unknown operation -- TABLE" message args))))

    dispatch))

(define (num-cmp a b)
  (cond
    ((< a b) -1)
    ((> a b) 1)
    (else 0)))

(define tree (make-tree num-cmp))

(check-false (tree 'lookup 'a))

(tree 'insert! 1 "one")
(tree 'insert! 2 "two")
(tree 'insert! 3 "three")

(check-equal? (tree 'lookup 1) "one")
(check-equal? (tree 'lookup 2) "two")
(check-equal? (tree 'lookup 3) "three")


(define (make-table cmp)
  ;; A table that can hold records under an arbitrary number of keys.
  (let ((table (mcons '*table* (make-tree cmp))))
    (define (lookup keys)
      (define (loop subtable keys)
        (if (null? keys)
          subtable
          ;; Look up the first key in the current subtable.
          (let ((record (subtable 'lookup (car keys))))
            (if (not record)
              false
              (loop record (cdr keys))))))
      (loop (mcdr table) keys))

    (define (insert! keys value)
      (define (loop subtable keys)
        (if (null? (cdr keys))
          (subtable 'insert! (car keys) value)
          (let ((record (subtable 'lookup (car keys))))
            (if record
              (loop record (cdr keys))
              (let ((new-subtable (make-tree cmp)))
                (subtable 'insert! (car keys) new-subtable)
                (loop new-subtable (cdr keys)))))))
      (loop (mcdr table) keys))

    (define (dispatch message . args)
      (cond
        ((eq? message 'lookup) (apply lookup args))
        ((eq? message 'insert!) (apply insert! args))
        (else (error "Unknown operation -- TABLE" message args))))

    dispatch))

(define table (make-table num-cmp))

(table 'insert! '(1 2 3) "one-two-three")
(check-equal? (table 'lookup '(1 2 3)) "one-two-three")

(table 'insert! '(1 2 4) "one-two-four")
(table 'insert! '(1 3) "one-three")
(table 'insert! '(2 5) "two-five")
(check-equal? (table 'lookup '(1 2 3)) "one-two-three")
(check-equal? (table 'lookup '(1 2 4)) "one-two-four")
(check-equal? (table 'lookup '(1 3)) "one-three")
(check-equal? (table 'lookup '(2 5)) "two-five")
