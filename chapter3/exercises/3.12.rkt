#lang racket

(require rackunit)

(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(define (append! l1 l2)
  (set-mcdr! (last-pair l1) l2)
  l1)

(define (last-pair ml)
  (cond ((empty? ml) (error "ml cannot be empty"))
        ((null? (mcdr ml)) ml)
        (else (last-pair (mcdr ml)))))

(define (mlist . args)
    (if (null? (cdr args))
        (mcons (car args) null)
        (mcons (car args)
               (apply mlist (cdr args)))))

(let ((l1 (mlist 1 2 3))
      (l2 (mlist 6 5 4)))
  (check-equal? l1 (mcons 1 (mcons 2 (mcons 3 null))))
  (check-equal? (last-pair l1) (mcons 3 null))
  (check-equal? (begin
                  (append! l1 l2)
                  l1)
                (mlist 1 2 3 6 5 4)))

;; Solution
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

;; x -> [ ● | ●-]-->[ ● | ╱ ]
;;        |           |
;;        v           v
;;        'a          'b
;;
;; y -> [ ● | ●-]-->[ ● | ╱ ]
;;        |           |
;;        v           v
;;        'c          'd
;;
;; z is a new list, append does not reuse pairs
;;
;; z -> [ ● | ●-]-->[ ● | ●-]-->[ ● | ●-]-->[ ● | ╱ ]
;;        |           |           |           |
;;        v           v           v           v
;;        'a          'b          'c          'd

z
;; (a b c d)
(cdr x)
;; '(b)

(define w (append! x y))

;; Now append not only reuses pairs but also mutates 
;; the last pair of the first list
;;
;; x,w -> [ ● | ●-]-->[ ● | ● ]
;;         |           |    |
;;         v           v    |
;;         'a          'b   |
;;                          |
;;         -----------------
;;        |
;;        v
;; y -> [ ● | ●-]-->[ ● | ╱ ]
;;        |           |
;;        v           v
;;        'c          'd

(cdr x)
;; '(b c d)
