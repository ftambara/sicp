#lang racket

;; Exercise 3.6 discussed generalizing the random-number generator to allow one
;; to reset the random-number sequence so as to produce repeatable sequences of
;; “random” numbers. Produce a stream formulation of this same generator that
;; operates on an input stream of requests to generate a new random number or
;; to reset the sequence to a specified value and that produces the desired
;; stream of random numbers. Don’t use assignment in your solution.

(require "../../modules/streams.rkt")

(define random-init 0)

;; Mimic Scheme's rand-update
(define (rand-update seed)
  (random-seed seed)
  (random 2147483647)) 

(define (random-stream requests)
  (define (next requests last-val)
    (define (dispatch m)
      (cond [(eq? m 'generate) (rand-update last-val)]
            [(and (pair? m) (eq? (car m) 'reset)) (rand-update (cdr m))]
            [else (error "Invalid argument" m)]))
    (if (empty? requests)
      empty-stream
      (let ([val (dispatch (stream-first requests))])
        (stream-cons val (next (stream-rest requests) val)))))
  (next requests random-init))


(define requests (list 'generate 'generate (cons 'reset 1) 'generate 'generate))
(define rand-stream (random-stream requests))
(stream-display rand-stream)
;; 883280686
;; 1106662596
;; 449302150
;; 184674760
;; 597930622
;; 'done
