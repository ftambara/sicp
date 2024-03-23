#lang racket

(require rackunit)

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define x null)
(check-eq? (count-pairs (cons x (cons x (cons x x)))) 3)
;;  a             b            c
;;  |             |            |
;;  └-->[ x | .-]-┴->[ . | .-]-┴->[ x | x ]

(define c (cons x x))
(define b (cons c c))
(define a (cons x b))
(check-eq? (count-pairs a) 4)
;;  a             b            c
;;  |             |            |
;;  └-->[ x | --]-┴->[ . | --]-┼->[ x | x ]
;;                     |       |
;;                     └-------┘

(define f (cons x x))
(define e (cons f f))
(define d (cons e e))
(check-eq? (count-pairs d) 7)
;;  d             e            f
;;  |             |            |
;;  └-->[ . | .-]-┼->[ . | --]-┼->[ x | x ]
;;        |       |    |       |
;;        └-------┘    └-------┘


(define (count-mpairs x)
  (if (not (mpair? x))
    0
    (+ (count-mpairs (mcar x))
       (count-mpairs (mcdr x))
       1)))
(define i (mcons x x))
(define h (mcons x i))
(define g (mcons x h))
(set-mcdr! i g)
; (count-mpairs g) produces an infinite loop
;; g             h            i
;; |             |            |
;; ├-->[ x | .-]-┴->[ x | .-]-┴->[ x | . ]
;; |                                   |
;; └-----------------------------------┘
