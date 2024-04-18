#lang racket

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c2)
    (half-adder a s sum c1)
    (or-gate c1 c2 c-out)))

(define (inverter in out)
  (define (inverter-proc)
    (let ((new-value
            (logical-not (get-signal in))))
      (after-delay! inverter-delay
                   (lambda ()
                     (set-signal! out new-value)))))
  (add-action! in inverter-proc))

(define (logical-not sig)
  (if (on? sig) on-sig off-sig))

(define (and-gate a1 a2 out)
  (define (and-gate-proc)
    (let ((new-value
            (logical-and (get-signal a1)
                         (get-signal a2))))
      (after-delay! and-gate-delay
                   (lambda ()
                     (set-signal! out new-value)))))
  (add-action! a1 and-gate-proc)
  (add-action! a2 and-gate-proc))

(define (make-wire)
  (let ((signal off-sig)
        (actions '()))
    (define (set-my-signal! new-value)
      (cond ((= signal new-value) 'done)
            (else (set! signal new-value)
                  (call-each actions))))
    (define (accept-action! proc)
      (set! actions (cons proc actions))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action)
  ((wire 'add-action!) action))

(define (call-each procs)
  (if (null? procs)
      'done
      (begin
        ((car procs))
        (call-each (cdr procs)))))
