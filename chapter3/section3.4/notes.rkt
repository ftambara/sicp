#lang racket


;; Serializers

(define (execute-parallel . procs) void)
;; Uncontrolled concurrent execution
(define x 10)
(execute-parallel
  (lambda () (set! x (* x x)))
  (lambda () (set! x (+ x 1))))

;; Serialized execution
(define serializer (make-serializer))
(execute-parallel
  (serializer (lambda () (set! x (* x x))))
  (serializer (lambda () (set! x (+ x 1)))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (serializer withdraw))
            ((eq? m 'deposit) (serializer deposit))
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    (if (> difference 0)
      (begin
        ((account1 'withdraw) difference)
        ((account2 'deposit) difference))
      (begin
        ((account1 'deposit) (- difference))
        ((account2 'withdraw) (- difference))))))

;; Why not provide a serialized exchange operation in the make-account
;; procedure, and avoid exposing the serializer?

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

;; > The mutex is a mutable object (here we’ll use a one-element list, which
;; > we’ll refer to as a _cell_)
;; Why not use a normal variable? They are just as mutable, as far as I know.
;; A pair is the closest thing to a pointer that we have seen so far, so
;; that may me a hint.

(define (make-mutex)
  (let ((cell (list false)))
    (define (clear! cell) (set-car! cell false))
    (define (test-and-set! cell)
      ;; test-and-set! simulates an atomic procedure where the access
      ;; to cell and its mutation happen simultaneously
      (if (car cell) true (begin (set-car! cell true) false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (my-make-mutex)
  (let ((cell false))
    (define (acquire!)
      ;; flaw: does not access and mutate simultaneously
      (if cell
        (acquire!)
        (set! cell true)))
    (define (release!)
      (set! cell false))
    (define (dispatch m)
      (cond ((eq? m 'acquire!) (acquire!))
            ((eq? m 'release!) (release!))
            (else (error "Invalid method MUTEX:" m))))
    dispatch))
