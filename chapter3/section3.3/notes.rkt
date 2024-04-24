#lang racket

(require compatibility/mlist)

;; “The desire to model systems composed of objects that have changing state
;; leads us to the need to modify compound data objects, as well as to
;; construct and select from them.”
;; I don't see how we jumped to compound data objects from that argument.

;; Mutable here probably refers to something slightly different that what I'm
;; used to from other languages. In this context, any bound variable that
;; has a procedure that can change its value is mutable. This is different
;; from my understanding of mutable in other languages, where it refers to
;; types that allow their values to be changed. I wouldn't have considered
;; instance variables to be mutable, or merely reassigning a variable to a
;; new value to be mutation.
;; These two views can be reconciled by considering that in other languages,
;; variables are mutable, but the values they reference may not be (for
;; example, integer values are immutable, while array values are mutable, even
;; if we don't affect the variable's reference).

;; Identity is tested by simply comparing pointers. Apparently, this is
;; not sufficient for more complex programs (I can't see why, though).

;; Queues
(provide
  queue-pop!
  queue-push!
  make-queue
  see-front)

(define (queue-pop! q)
  (if (empty-queue? q)
    (error "Cannot pop from empty queue")
    (let ((old-front (front-ptr q)))
      (set-front-ptr! q (mcdr (front-ptr q)))
      (mcar old-front))))

(define (see-front q)
  (front-ptr q))

(define (queue-push! q item)
  (let ((new-pair (mcons item null)))
    (if (empty-queue? q)
      (begin
        (set-front-ptr! q new-pair)
        (set-rear-ptr! q new-pair)
        q)
      (begin
        (set-mcdr! (rear-ptr q) new-pair)
        (set-rear-ptr! q new-pair)
        q))))

(define (front-ptr q) (car q))

(define (rear-ptr q) (cdr q))

(define (set-front-ptr! q pair)
  (set-mcar! q pair))

(define (set-rear-ptr! q pair)
  (set-mcdr! q pair))

(define (make-queue)
  ;; Both underlying lists need to be mutable
  (cons (mlist) (mlist)))

(define (empty-queue? q)
  (null? (front-ptr q)))

;; Tables

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
      (mcdr record)
      false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-mcdr! record value)
      ;; new key
      (set-mcdr! table (mcons (mcons key value) (mcdr table)))))
  'ok)

(define (make-table)
  (mcons '*table* null))

(define (lookup-2d key1 key2 table)
  (let ((subtable (assoc key1 (mcdr table))))
    (if subtable
      (let ((record (assoc key2 (mcdr subtable))))
        (if record
          (mcdr record)
          false))
      false)))

(define (insert-2d! key1 key2 value table)
  (let ((subtable (assoc key1 (mcdr table))))
    (if subtable
      (let ((record (assoc key2 (mcdr subtable))))
        (if record
          (set-mcdr! record value)
          ;; new sub-level key
          (set-mcdr! subtable (mcons (mcons key2 value) (mcdr subtable)))))
      ;; new top level key
      (set-mcdr! table
                 (mcons (list key1 (mcons key2 value))
                       (mcdr table)))))
  'ok)

(define (make-table-proc)
  (let ((table (mcons '*table* null)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr table))))
        (if subtable
          (let ((record (assoc key2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))
    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (mcdr table))))
        (if subtable
          (let ((record (assoc key2 (mcdr subtable))))
            (if record
              (set-mcdr! record value)
              ;; new sub-level key
              (set-mcdr! subtable (mcons (mcons key2 value) (mcdr subtable)))))
          ;; new top level key
          (set-mcdr! table
                     (cons (list key1 (cons key2 value))
                           (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "invalid method" m))))
    dispatch))

(define operation-table (make-table))
(define put (operation-table 'insert!))
(define get (operation-table 'lookup))

;; Digital circuit simulation

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

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

(define (make-wire)
  (define (call-each procs)
    (if (null? procs)
      'done
      (begin ((car procs))
             (call-each (cdr procs)))))

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
      (newline)
      (printf "~a: ~a, ~a\n"
              name 
              (current-time the-agenda)
              (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)

(define (make-time-segment time queue)
  (mcons time queue))

(define (segment-time segment) (mcar segment))
(define (segment-queue segment) (mcdr segment))

(define (make-agenda) (mcons 0 (mlist)))
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
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
      (insert-queue! (segment-queue (mcar segments))
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
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda
                         (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))


;; Propagation of constraints

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier! c w u)
    (multiplier! v x u)
    (adder! v y f)
    (constant! 9 w)
    (constant! 5 x)
    (constant! 32 y)
    'ok))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (adder! a1 a2 s)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! s (+ (get-value a1) (get-value a2)) me))
          ((and (has-value? s) (has-value? a1))
           (set-value! a2 (- (get-value s) (get-value a1)) me))
          ((and (has-value? s) (has-value? a2))
           (set-value! a1 (+ (get-value s) (get-value a2)) me))
          (else 'do-nothing)))
  (define (process-forget-value)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (forget-value! s me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect! a1 me)
  (connect! a2 me)
  (connect! s me)
  me)

(define (multiplier! m1 m2 p)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! p 0))
          ((and (has-value? m1) (has-value? m2))
           (set-value! p (+ (get-value m1) (get-value m2)) me))
          ((and (has-value? p) (has-value? m1))
           (set-value! m2 (/ (get-value p) (get-value m1)) me))
          ((and (has-value? p) (has-value? m2))
           (set-value! m1 (+ (get-value p) (get-value m2)) me))
          (else 'do-nothing)))
  (define (process-forget-value)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (forget-value! p me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  (connect! m1 me)
  (connect! m2 me)
  (connect! p me)
  me)

(define (constant! value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect! connector me)
  (set-value! connector value me)
  me)

(define (connector-probe! name connector)
  (define (process-new-value)
    (printf "Probe: ~a = ~a\n" name (get-value connector)))
  (define (process-forget-value)
    (printf "Probe: ~a = ?\n" name))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect! connector me)
  me)

(define (make-connector)
  (let ((value null)
        (informant null)
        (constraints '()))
    (define (notify-each-except except boxes proc)
      (cond ((empty? boxes) 'done)
            ((not (eq? (car boxes) except))
             (proc (car boxes))
             (notify-each-except except (cdr boxes) proc))
            (else (notify-each-except except (cdr boxes) proc))))

    (define (has-value?) (null? informant))

    (define (set-value! new-value who)
      (cond ((null? informant)
             (set! value new-value)
             (set! informant who)
             (notify-each-except who constraints inform-about-value))
            ((not (= value new-value))
             (error "Contradiction" value new-value))
            (else 'ignored)))

    (define (forget-value! retractor)
      (cond ((null? informant)
             (error ("Connector has no informant")))
            ((eq? retractor informant)
             (set! value null)
             (set! informant null)
             (notify-each-except retractor constraints inform-about-no-value))
            (else 'wrong-informant)))

    (define (connect! new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints
          (cons new-constraint constraints))
        'already-connected)
      (if (has-value?)
        (inform-about-value new-constraint)
        'skipped)
      'done)

    (define (dispatch m . args)
      (cond ((eq? m 'get-value) value)
            ((eq? m 'has-value?) has-value?)
            ((eq? m 'set-value!) (apply set-value! args))
            ((eq? m 'forget-value!) (apply forget-value! args))
            ((eq? m 'connect!) (apply connect! args))
            (else (error "Unknown operation: CONNECTOR" m))))

    dispatch))


(define (get-value connector)
  (connector 'get-value))

(define (has-value? connector)
  (connector 'has-value?))

(define (set-value! connector new-value informant)
  (connector 'set-value! new-value informant))

(define (forget-value! connector retractor)
  (connector 'forget-value! retractor))

(define (connect! connector new-constraint)
  (connector 'connect! new-constraint))

(connector-probe! "Celsius temp" C)
(connector-probe! "Farenheit temp" F)
(set-value! C 25 'user)

(set-value! F 212 'user)

(forget-value! C 'user)

(set-value! F 212 'user)
