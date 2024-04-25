#lang racket

(define x 10)
;; (parallel-execute (lambda () (set! x (* x x)))
;;                   (lambda () (set! x (* x x x))))

;; procA: readA1 readA2 writeA
;; procB: readB1 readB2 readB3 writeB

;; This is a very tangled way of simulating the interleaving of the two processes
(define (ordered-combinations procsA procsB)
  ;; ordered-combinations generates all possible interleavings of two processes,
  ;; keeping internal order
  (define (fold-insert count main default locations)
    (if (null? locations)
      default
      (if (= count (car locations))
        (cons (car main) (fold-insert (add1 count) (cdr main) default (cdr locations)))
        (cons (car default) (fold-insert (add1 count) main (cdr default) locations)))))

  (define (loop results locations)
    (if (null? locations)
      results
      (loop (cons (fold-insert 0 procsB procsA (car locations)) results)
            (cdr locations))))

  (loop '() (combinations (range (+ (length procsA)
                                    (length procsB)))
                          (length procsB))))

;; Variables and functions needed for the simulation
(define unique '())
(define read-exes-a '())
(define read-exes-b '())
(define (readA) (set! read-exes-a (cons x read-exes-a)))
(define (readB) (set! read-exes-b (cons x read-exes-b)))
(define (writeA) (set! x (foldl * 1 read-exes-a)))
(define (writeB) (set! x (foldl * 1 read-exes-b)))
(define (reset)
  (if (not (member x unique))
      (set! unique (cons x unique))
      'ignore)
  (set! x 10)
  (set! read-exes-a '())
  (set! read-exes-b '()))

(define (simulate-interleaving procs-list reset-fn)
  (define (execute-procs procs)
    (if (null? procs)
      (reset-fn)
      (begin
        ((car procs))
        (execute-procs (cdr procs)))))
  (if (null? procs-list)
    'done
    (begin
      (execute-procs (car procs-list))
      (simulate-interleaving (cdr procs-list) reset-fn))))

(define procsA (list readA readA writeA))
(define procsB (list readB readB readB writeB))
(define combs (ordered-combinations procsA procsB))
(simulate-interleaving combs reset)

unique

;; Simpler way of analyzing all possible combinations in this case

;; Values procA can set
;; 10 * 10          = 100
;; 10 * 1000         = 1000
;; 1000 * 1000        = 1000000

;; Values procB can set
;; 10 * 10 * 10     = 1000
;; 10 * 10 * 100    = 10000
;; 10 * 100 * 100   = 100000
;; 100 * 100 * 100  = 1000000

;; Unique final values:
;;  100
;;  1000
;;  10000
;;  100000
;;  1000000


;; (define s (make-serializer))
;; (parallel-execute (s (lambda () (set! x (* x x))))
;;                   (s (lambda () (set! x (* x x x)))))

;; Only two possibilities remain
;; 10 * 10  = 100 -> 100 * 100 * 100 = 1000000
;; 10 * 10 * 10 = 1000 -> 1000 * 1000 = 1000000
;; Both yield 10^6
