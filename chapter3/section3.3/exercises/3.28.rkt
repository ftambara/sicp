#lang racket

(define add-action! void)
(define after-delay void)
(define or-gate-delay void)
(define set-signal! void)
(define get-signal void)

(define (or-gate! in1 in2 out)
  (define (logic-or s1 s2)
    (if (or (= s1 1) (= s2 1))
      1
      0))
  (define (or-gate-proc!)
    (after-delay
      or-gate-delay
      (lambda ()
        (set-signal! out (logic-or (get-signal in1)
                                   (get-signal in2))))))
  (add-action! in1 or-gate-proc!)
  (add-action! in2 or-gate-proc!)
  'ok)
