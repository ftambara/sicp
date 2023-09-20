#lang racket

(require "2.67.rkt"
         rackunit)


;; From the book
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


;; Solution
(define (integer-list->string integer-list)
  (foldr
   (lambda (integer acc)
     (string-append (number->string integer) acc))
   ""
   integer-list))

(define (string->symbol-list str)
  (map (lambda (ch) (string->symbol (string ch)))
       (string->list str)))

(define (encode-symbol sym code-tree)
  (cond ((and (leaf? code-tree) (equal? sym (symbol-leaf code-tree)))
         '())
        ((member sym (symbols (left-branch code-tree)))
         (cons 0 (encode-symbol sym (left-branch code-tree))))
        ((member sym (symbols (right-branch code-tree)))
         (cons 1 (encode-symbol sym (right-branch code-tree))))
        (else (error "Symbol is not encoded by tree\n"))))

;; Tests
(define code "00111000111110010")

(define short-code-tree
  (make-code-tree
   (make-leaf 'A 8)
   (make-code-tree
    (make-leaf 'B 3)
    (make-code-tree
     (make-leaf 'C 1)
     (make-code-tree
      (make-leaf 'D 1)
      (make-leaf 'E 1))))))

(check-equal?
 (integer-list->string
  (encode (string->symbol-list (my-decode code short-code-tree))
          short-code-tree))
 code)
