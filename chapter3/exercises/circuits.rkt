#lang racket

(require "queues.rkt")

(provide
  propagate!
  reset-time!
  probe!
  make-wire
  get-signal
  set-signal!
  inverter!
  and-gate!
  or-gate!
  half-adder!
  full-adder!)

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (call-each procs)
  (if (null? procs)
    'done
    (begin ((car procs))
           (call-each (cdr procs)))))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))

    (define (add-action! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m . args)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) (set-signal! (car args)))
            ((eq? m 'add-action!) (add-action! (car args)))
            (else (error "Unknown operation: WIRE" m))))

    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire value) (wire 'set-signal! value))
(define (add-action! wire proc) (wire 'add-action! proc))

(define (half-adder! a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate! a b d)
    (and-gate! a b c)
    (inverter! c e)
    (and-gate! d e s)))

(define (full-adder! a b c-in sum c-out)
  (let ((s1 (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder! b c-in s1 c1)
    (half-adder! a s1 sum c2)
    (or-gate! c2 c1 c-out)))

(define (inverter! in out)
  (define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal" s))))
  (define (inverter-action-proc!)
    (after-delay!
      inverter-delay
      (lambda () (set-signal! out (logical-not (get-signal in))))))
  (add-action! in inverter-action-proc!)
  'ok)

(define (and-gate! a1 a2 out)
  (define (logical-and s1 s2)
    (if (and (= s1 1) (= s2 1))
      1
      0))
  (define (and-action-proc!)
    (after-delay!
      and-gate-delay
      (lambda ()
        (set-signal! out
                     (logical-and (get-signal a1)
                                  (get-signal a2))))))
  (add-action! a1 and-action-proc!)
  (add-action! a2 and-action-proc!)
  'ok)

(define (or-gate! in1 in2 out)
  (define (logic-or s1 s2)
    (if (or (= s1 1) (= s2 1))
      1
      0))
  (define (or-gate-proc!)
    (after-delay!
      or-gate-delay
      (lambda ()
        (set-signal! out (logic-or (get-signal in1)
                                   (get-signal in2))))))
  (add-action! in1 or-gate-proc!)
  (add-action! in2 or-gate-proc!)
  'ok)

(define (after-delay! delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate!)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate!))))

(define (probe! name wire)
  (add-action!
    wire
    (lambda ()
      (printf "~a: ~a, ~a\n"
              name
              (current-time the-agenda)
              (get-signal wire)))))

(define (make-time-segment time queue)
  (mcons time queue))

(define (segment-time segment) (mcar segment))
(define (segment-queue segment) (mcdr segment))

(define (make-agenda) (mcons 0 '()))
(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time)
  (set-mcar! agenda time))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))
(define (first-segment agenda)
  (mcar (segments agenda)))
(define (rest-segments agenda)
  (mcdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (queue-push! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
      (queue-push! (segment-queue (mcar segments))
                     action)
      (let ((rest (mcdr segments)))
        (if (belongs-before? rest)
          (set-mcdr!
            segments
            (mcons (make-new-time-segment time action)
                  (mcdr segments)))
          (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments!
        agenda
        (mcons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (queue-pop! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda))
      'done)))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (see-front (segment-queue first-seg)))))

(define the-agenda (make-agenda))

(define (reset-time!)
  (set-current-time! the-agenda 0))
