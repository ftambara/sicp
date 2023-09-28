#lang racket

(require "../../utils.rkt"
         "2.68.rkt"
         "2.69.rkt")


;; (define (encode message tree)
;;   (if (null? message)
;;       '()

;;      append is O(n) where n is the length of the encoded symbol
;;       (append (encode-symbol (car message) tree)
;;               (encode (cdr message) tree))))

;; (define (encode-symbol sym code-tree)
;;   (cond ((and (leaf? code-tree) (equal? sym (symbol-leaf code-tree)))
;;          '())

;;   encode-symbol worst checks for membership in the symbols of each
;;      node for every level, which leads to (I think) O(n^2)
;;      behavior (linear time for each check, n checks), where n is
;;      the number of symbols.

;;         ((member sym (symbols (left-branch code-tree)))
;;          (cons 0 (encode-symbol sym (left-branch code-tree))))
;;         ((member sym (symbols (right-branch code-tree)))
;;          (cons 1 (encode-symbol sym (right-branch code-tree))))
;;         (else (error "Symbol is not encoded by tree: " sym))))

;; I expect encode to run in O(n^2) time, where n is the number of
;; distinct symbols in the tree.


;; Experiments
(define (huffman-tree-n n)
  (generate-huffman-tree (generate-symbols n)))

(define (generate-symbols n)
  (if (= n 0)
      '()
      (cons (list (string->symbol (number->string n)) (expt 2 (- n 1)))
            (generate-symbols (- n 1)))))

(define code-tree-5 (huffman-tree-n 5))
(define code-tree-10 (huffman-tree-n 10))
(define code-tree-20 (huffman-tree-n 20))
(define code-tree-40 (huffman-tree-n 40))


(time-procedure
 (lambda () (encode (make-list 10000 (string->symbol "1")) code-tree-5))
 10)
; Average 5.5 ms

(time-procedure
 (lambda () (encode (make-list 10000 (string->symbol "1")) code-tree-10))
 10)
; Average 4.5 ms

(time-procedure
 (lambda () (encode (make-list 10000 (string->symbol "1")) code-tree-20))
 10)
; Average 10.6 ms

(time-procedure
 (lambda () (encode (make-list 10000 (string->symbol "1")) code-tree-40))
 10)
; Average 28.5 ms

;; Unexpectedly, the time to encode the least-frequent symbol seems
;; to be O(n). By looking at a generated tree, I realized that the
;; least frequent symbol is always first in the list of symbols of
;; each node, so checking for membership is only O(n).
