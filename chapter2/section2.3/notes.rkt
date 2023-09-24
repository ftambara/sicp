#lang racket

(require rackunit)


(define (memq item sequence)
  (cond ((null? sequence) false)
        ((eq? item (car sequence)) sequence)
        (else (memq item (cdr sequence)))))

(check-false (memq 'apple '(banana pear orange)))
(check-false (memq 'apple '(bananas pears apples)))
(check-equal? (memq 'apple '(banana pear apple)) '(apple))
(check-equal?
 (memq 'apple '(banana (apple peach) apple pear)) '(apple pear))

(define (variable? e)
  (symbol? e))
;; (variable? 'x) => #t
;; (variable? 'ab) => #t
;; (variable? '(+ ab x)) => #f

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        (else (list '* m1 m2))))

(define (sum? e)
  (and (list? e) (eq? (car e) '+)))

(define (addend e)
  (cadr e))

(define (augend e)
  (caddr e))

(define (product? e)
  (and (list? e) (eq? (car e) '*)))

(define (multiplier e)
  (cadr e))

(define (multiplicand e)
  (caddr e))


;; The trick of developing with abstractions first without an explicit
;; implementation is pretty neat. It forces you to design good
;; abstraction barriers, eliminating the temptation of reaching into
;; the data structures, as yet unknown.

(define (derive expr variable)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr variable) 1 0))
        ((sum? expr)
         (make-sum (derive (addend expr) variable)
                   (derive (augend expr) variable)))
        ((product? expr)
         (let ((m1 (multiplier expr))
               (m2 (multiplicand expr)))
           (make-sum (make-product m1 (derive m2 variable))
                     (make-product (derive m1 variable) m2))))
        (else (error "Cannot derive " expr))))

;; Examples
(check-equal? (derive '(+ x 3) 'x) 1)
(check-equal? (derive '(* x y) 'x) 'y)
(check-equal?  (derive '(* (* x y) (+ x 3)) 'x)
               '(+ (* x y) (* y (+ x 3))))

;; Sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (define (iter new-set rest)
    (cond ((null? rest) new-set)
          ((element-of-set? (car rest) set2)
           (iter (cons (car rest) new-set) (cdr rest)))
          (else (iter new-set (cdr rest)))))
  (iter '() set1))


;; Sets as ordered lists

(define (element-of-ord-set? x ord-set)
  (cond ((null? ord-set) false)
        ((= x (car ord-set)) true)
        ((< x (car ord-set)) false)
        (else (element-of-ord-set? x (cdr ord-set)))))


;; Sets as trees

(define (entry-set tree)
  (car tree))

(define (left-branch-set tree)
  (cadr tree))

(define (right-branch-set tree)
  (caddr tree))

(define (make-set-tree entry left right)
  (list entry left right))

(define (element-of-set-tree? x set)
  (if (null? set) false
      (cond ((= x (entry-set set)) true)
            ((< x (entry-set set))
             (element-of-set-tree? x (left-branch-set set)))
            ((> x (entry-set set))
             (element-of-set-tree? x (right-branch-set set))))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-set-tree x null null))
        ((= x (entry-set set)) set)
        ((< x (entry-set set))
         (make-set-tree (entry-set set)
                        (adjoin-set-tree x (left-branch-set set))
                        (right-branch-set set)))
        ((> x (entry-set set))
         (make-set-tree (entry-set set)
                        (left-branch-set set)
                        (adjoin-set-tree x (right-branch-set set))))))


;; Huffman trees

(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

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

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set-weight x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set-weight x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set-weight (make-leaf (car pair)   ; symbol
                                      (cadr pair)) ; frequency
                           (make-leaf-set (cdr pairs))))))
