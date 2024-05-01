#lang racket

;; Examine the stream (pairs integers integers). Can you make any general
;; comments about the order in which the pairs are placed into the stream? For
;; example, approximately how many pairs precede the pair (1, 100)? the pair
;; (99, 100)? the pair (100, 100)? (If you can make precise mathematical
;; statements here, all the better. But feel free to give more qualitative
;; answers if you ﬁnd yourself getting bogged down.)

(require rackunit)

(define integers (stream-cons 1 (stream-map add1 integers)))

(define (pairs s t)
  (let ([s0 (stream-first s)])
    (stream-cons
      (cons s0 (stream-first t))
      (interleave
        (stream-map (lambda (ti) (cons s0 ti)) (stream-rest t))
        (pairs (stream-rest s) (stream-rest t))))))

(define (interleave s1 s2)
  (if (stream-empty? s1)
    s2
    (stream-cons (stream-first s1)
                 (interleave s2 (stream-rest s1)))))

(define int-pairs (pairs integers integers))

;; With a 1-based index
;;
;; 1: S1,T1 | S1,T2   S1,T3   S1,T4   S1,T5   ...
;;          |------------------------------------
;; 2:       | S2,T2 | S2,T3   S2,T4   S2,T5   ...
;;          |       |----------------------------
;; 3:       |       | S3,T3 | S3,T4   S3,T5   ...
;;          |       |       |--------------------
;; 4:       |       |       | S4,T4 | S4,T5   ...
;;          |       |       |       |------------
;; 5:       |       |       |       | S5,T5 | ...
;;          |       |       |       |   ... |

;; i j   pos  series
;; -----------------
;; 1 1     1       1
;; 1 2     2       1
;; 2 2     3       2
;; 1 3     4       1
;; 2 3     5       2
;; 1 4     6       1
;; 3 3     7       3
;; 1 5     8       1
;; 2 4     9       2

;; 1st series: one element every two elements of the main stream.
;; 2nd series: one element every four elements of the main stream.
;; 3rd series: one element every eight elements of the main stream.
;; n-th series: one element every 2^n elements of the main stream.

;; Head position:
;; 1,1: 1
;; 2,2: 3 (+2) = 1+2
;; 3,3: 7 (+4) = 1+2+4
;; k,k: 1+2+...+2^(k-1)

;; pos(i, j) = sum(k=1, i, 2^(k-1))         <-- Pos. of head of the series
;;             + 1/2 (2^i)       if j>i     <-- Pos. of the series' 2nd elem.
;;             + (2^i) * (j-i-1) if j-1>i   <-- Pos. of the series' (j-2)th elem.

;; With the help of Wolfram Alpha, because I don't
;; remember geometric series well enough:
;; pos(i, j) = 2^i - 1
;;             + 1/2 (2^i)       if j>i
;;             + (2^i) * (j-i-1) if j-1>i

;; Therefore to calculate pos(1, 100)
;; 100 > 1 && 99 > 1
;; pos(1, 100) = 2^1 -1 + 1/2 2 + 2*(98) = 1 + 1 + 196 = 198

(check-equal? (stream-ref int-pairs (sub1 198)) (cons 1 100))

;; pos(99, 100)
;; 100 > 99 && not 99 > 99
;; pos(99, 100) = 2^99 - 1 + 1/2 2^99 = (3/2 2^99) - 1 ~= 9.5 10^29

;; pos(100, 100)
;; not 100 > 100 && not 99 > 100
;; pos(100, 100) 2^100 -1 ~= 1.27 10^30
