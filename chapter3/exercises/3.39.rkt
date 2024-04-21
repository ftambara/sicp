#lang racket

(define x 10)
(define s (make-serializer))
(parallel-execute
  (lambda () (set! x ((s (lambda () (* x x))))))  ;; procA
  (s (lambda () (set! x (+ x 1)))))               ;; procB

;; For procA: [readA1 readA2] writeA
;; For procB: [readB writeB]
;;  Events between brackets are serialized and therefore
;;      cannot run concurrently

;; 101: P1 sets x to 100 and then P2 increments x to 101.               OK
;;      [readA1 readA2] writeA [readB writeB]
;; 121: P2 increments x to 11 and then P1 sets x to x * x.              OK
;;      [readB writeB] [readA1 readA2] writeA
;; 110: P2 changes x from 10 to 11 between the two times that
;;  P1 accesses the value of x during the evaluation of (* x x).        X
;; 11:  P2 accesses x, then P1 sets x to 100, then P2 sets x.           X
;; 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.    OK
;;      [readA1 readA2] [readB writeB] writeA
