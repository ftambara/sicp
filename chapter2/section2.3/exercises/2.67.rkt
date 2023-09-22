#lang racket

(require rackunit)

(provide symbols
         weight
         make-code-tree
         left-branch
         right-branch
         make-leaf
         leaf?
         symbol-leaf
         my-decode
         decode)


(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; Solution

(define (string->integer-list str)
  (map (lambda (char) (string->number (string char)))
       (string->list str)))

(define (symbol-list->string seq)
  (foldr (lambda (sym acc) (string-append (symbol->string sym) acc))
         ""
         seq))

(define (my-decode bits tree)
  (define (rec bit-list sub-tree)
    (cond ((null? bit-list) (list (symbol-leaf sub-tree)))
          ((leaf? sub-tree)
           (cons (symbol-leaf sub-tree) (rec bit-list tree)))
          (else (if (= (car bit-list) 0)
                    (rec (cdr bit-list) (left-branch sub-tree))
                    (rec (cdr bit-list) (right-branch sub-tree))))))
  (symbol-list->string (rec (string->integer-list bits) tree)))

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

(check-equal? (my-decode "00111000111110010" short-code-tree)
              "AADAAEBAB")
