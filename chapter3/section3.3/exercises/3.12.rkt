#lang racket

(require rackunit)

(provide mappend! last-pair mlist)

(define (mappend x y)
  (if (null? x)
    y
    (mcons (mcar x) (mappend (mcdr x) y))))

(define (mappend! l1 l2)
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
                  (mappend! l1 l2)
                  l1)
                (mlist 1 2 3 6 5 4)))

;; Solution
(define x (mlist 'a 'b))
(define y (mlist 'c 'd))
(define z (mappend x y))

;; x -> [ ● | ●-]-->[ ● | ╱ ]
;;        |           |
;;        v           v
;;        'a          'b
;;
;; z -> [ ● | ●-]-->[ ● | ● ]
;;        |           |    |
;;        v           v    |
;;        'a          'b   |
;;                         |
;;                         |
;;                         v
;;                    y -> [ ● | ●-]-->[ ● | ╱ ]
;;                           |           |
;;                           v           v
;;                           'c          'd

;; z
;; (a b c d)

;; (mcdr x)
;; '(b)

(define w (mappend! x y))

;; x,w -> [ ● | ●-]-->[ ● | ● ]
;;         |           |    |
;;         v           v    |
;;         'a          'b   |
;;                          |
;;                          |
;;                          v
;;                      y -> [ ● | ●-]-->[ ● | ╱ ]
;;                             |           |
;;                             v           v
;;                             'c          'd

;; (mcdr x)
;; '(b c d)
