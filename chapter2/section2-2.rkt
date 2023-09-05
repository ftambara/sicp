#lang racket

(define (nth-list list_ pos)
    (cond ((= pos 0) (car list_))
          ((or (< pos 0) (null? (cdr list_))) (error "Out of bounds"))
          (else (nth-list (cdr list_) (- pos 1)))))

(define (append-lists list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append-lists (cdr list1) list2))))

(require "exercises/2.17.rkt")
(require "exercises/2.18.rkt")
(require "exercises/2.19.rkt")
(require "exercises/2.20.rkt")


(define (map-list func list_)
    (if (null? list_)
        list_
        (cons (func (car list_)) (map-list (cdr list_) func))))

(define (scale-list list_ factor)
    (map-list (lambda (x) (* factor x)) list_))


(require "exercises/2.21.rkt")
(require "exercises/2.22.rkt")
(require "exercises/2.23.rkt")


(define (length-list list_)
    (define (iter count remain)
        (if (null? remain)
            count
            (iter (+ count 1) (cdr remain))))
    (iter 0 list_))

(define (count-leaves tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) 1)
          (else
            (+ (count-leaves (car tree))
               (count-leaves (cdr tree))))))

(define (scale-tree tree factor)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (scale-tree sub-tree factor)
                (* factor sub-tree)))
         tree))

(define (filter predicate sequence)
    (cond ((null? sequence) sequence)
          ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(filter odd? '(1 2 3 5 90 99))
; => (1 3 5 99)

; (define (accumulate op initial sequence)
;     (if (null? sequence)
;         initial
;         (accumulate op (op (car sequence) initial) (cdr sequence))))
; This implementation I thought of doesn't work for an op like cons,
; since it reverses the intended result

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))


(accumulate + 0 (list 1 2 3 4 5))
; => 15
(accumulate * 1 (list 1 2 3 4 5))
; => 120
(accumulate cons null (list 1 2 3 4 5))
; => (1 2 3 4 5)

(define (enumerate-interval low high)
    (if (> low high)
        null
        (cons low (enumerate-interval (add1 low) high))))

(enumerate-interval 2 7)
; (2 3 4 5 6 7)

(define (enumerate-tree tree)
    (cond ((null? tree) tree)
          ((list? tree)
            (append (enumerate-tree (car tree))
                    (enumerate-tree (cdr tree))))
          (else (list tree))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; (1 2 3 4 5)

(define (square x)
    (* x x))

(define (fib n)
    (define (iter two_ago one_ago count)
        (if (= count n)
            (+ two_ago one_ago)
            (iter one_ago (+ two_ago one_ago) (+ count 1))))
    (iter 1 0 0))

(define (sum-odd-squares tree)
    (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
    ; Why use an identity operation like this accumulate?
    ; Is the consistency worth it?
    (accumulate
        cons '() (filter even? (map fib (enumerate-interval 0 n)))))

; (define (enumerate-pairs n)
;     (accumulate
;         append
;         null
;         (map (lambda (i)
;                 (map (lambda (j) (list i j))
;                      (enumerate-interval 1 (- i 1))))
;              (enumerate-interval 1 n))))

(define (flatmap proc seq)
    (accumulate append null (map proc seq)))

(define (enumerate-pairs n)
    ; Generate a list of lists (pairs) for each i,
    ; Flatten all pairs into the same top-level list
    (flatmap (lambda (i)
                (map (lambda (j) (list i j))
                     (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(enumerate-pairs 4)

(define (prime? number)
    ; Use non-deterministic Fermat's test
    (define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)
                (remainder (square (expmod base (/ exp 2) m)) m))
              (else
                (remainder (* base (expmod base (- exp 1) m)) m))))
    (define (fermat-test n times)
        (define (try-it a)
            (= (expmod a n n) a))
        (or (= times 0)
            (and (try-it (+ 1 (random (- n 1))))
                 (fermat-test n (- times 1)))))
    (fermat-test number (max number 100)))

(define (prime-sum? couple)
    (prime? (+ (car couple) (cadr couple))))

(define (make-couple-sum-triple couple)
    (list (car couple) (cadr couple) (+ (car couple) (cadr couple))))

(define (prime-sum-couples n)
    (map
        make-couple-sum-triple
        (filter prime-sum? (enumerate-pairs n))))

(prime-sum-couples 6)

; (define (remove elem sequence)
;     (cond ((null? sequence) null)
;           ((= elem (car sequence)) (cdr sequence))
;           (else (cons (car sequence) (remove elem (cdr sequence))))))

(define (remove elem sequence)
    (filter (lambda (x) (not (= x elem))) sequence))

(define (permutations sequence)
    (if (null? sequence)
        (list null)
        (flatmap
            (lambda (prefix)
                (map (lambda (sub-perm) (cons prefix sub-perm))
                     (permutations (remove prefix sequence))))
            sequence)))

(permutations '(1 2 3))
