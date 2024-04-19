#lang racket

(provide
  adder!
  multiplier!
  constant!
  connect!
  connector-probe!
  celsius-fahrenheit-converter!
  forget-value!
  get-value
  has-value?
  make-connector
  set-value!)

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
           (set-value! a1 (- (get-value s) (get-value a2)) me))
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
           (set-value! p 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! p (* (get-value m1) (get-value m2)) me))
          ((and (has-value? p) (has-value? m1))
           (set-value! m2 (/ (get-value p) (get-value m1)) me))
          ((and (has-value? p) (has-value? m2))
           (set-value! m1 (/ (get-value p) (get-value m2)) me))
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

    (define (has-value?) (not (null? informant)))

    (define (set-value! new-value who)
      (cond ((null? informant)
             (set! value new-value)
             (set! informant who)
             (notify-each-except who constraints inform-about-value))
            ((not (= value new-value))
             (error "Contradiction" value new-value))
            (else 'ignored)))

    (define (forget-value! retractor)
      (cond ((eq? retractor informant)
             (set! value null)
             (set! informant null)
             (notify-each-except retractor constraints inform-about-no-value))
            (else 'ignored)))

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
            ((eq? m 'has-value?) (has-value?))
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

(define (celsius-fahrenheit-converter! c f)
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
