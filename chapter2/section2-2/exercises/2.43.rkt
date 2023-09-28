#lang racket

(require
  "2.40.rkt"
  "2.42.rkt"
  "../../utils.rkt")

(define length-sum 0)
(define (sum-length seq)
  (set! length-sum (+ length-sum (length seq)))
  seq)

(define (queens-slow board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? (car positions) (cdr positions)))
         (sum-length (flatmap
                      (lambda (proposed-row)
                        (map (lambda (rest-of-queens)
                               (adjoin-position proposed-row k rest-of-queens))
                             (queens-cols (- k 1))))
                      (enumerate-interval 1 board-size))))))
  (queens-cols board-size))

(define (echo-length sequence)
  (display (length sequence))
  (newline)
  sequence)

(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? (car positions) (cdr positions)))
         (sum-length (flatmap
                      (lambda (rest-of-queens)
                        (map (lambda (proposed-row)
                               (adjoin-position proposed-row k rest-of-queens))
                             (enumerate-interval 1 board-size)))
                      (queens-cols (- k 1)))))))
  (queens-cols board-size))

(display "Slow flatmap:\n")
(time-procedure (lambda () (queens-slow 8)) 1)
(printf "Length sum: ~a\n" length-sum)
; Average time: 2.4 s
; Length sum: 50.9 M

(set! length-sum 0)
(display "\nNormal flatmap:\n")
(time-procedure (lambda () (queens 8)) 5)
(printf "Length sum: ~a\n" length-sum)
; Average time: 0.6 ms
; Length sum: 78.6 k

; Empirically, the slow version took around 4000 times longer than
; the normal version.

; The slowness of this version is due to recursivity. By placing
; the call to queens-cols in the inner loop, we solve the k-1
; previous columns many times instead of once. Furthermore, we
; solve the previous columns for proposed positions that are
; going to be filtered out. With the faster version, we only
; solve the k-1 problem once for each k, and only generate up
; to board-size unnecessary positions per column.

; I expect the 'fast' case to run in O(n^n) time, where n is the size
; of the board. The reasoning is that for each queen position, we
; try n new positions. This does not take into account the filtering
; part, hoping that it's the smaller effect.
; For the slow case it's even harder to tell. I suspect that
; since we solve the n-1 problems n times, and before that the n-2
; problems n-1 times, we get something like O(n^n^n).

; Checking the lengths of the intermediate flatmap results for the
; normal case, I see that the growth is larger than n^3, but smaller
; than n^n
