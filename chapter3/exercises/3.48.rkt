#lang racket

;; > Explain in detail why the deadlock-avoidance
;; method described above, (i.e., the accounts are numbered,
;; and each process attempts to acquire the smaller-numbered
;; account ﬁrst) avoids deadlock in the exchange problem.

;; If, when interacting with multiple accounts, we always lock them in a 
;; consistent order at any given time, then we will avoid deadlocks.
;; Suppose process 1 attempts to exchange a1 with a2, while process 2 tries
;; to do the opposite, that is, exchange a2 with a1. If a1 is taken to be the one
;; that is always locked first, and process 1 arrives first, then it will acquire
;; the log on a1. When process 2 arrives, it will request the lock on a1, so it
;; will be made to wait for process 1 to finish using it. The same will happen on
;; a2. Deadlocks occur when processes go in conflicting orders.

;; For a more general case, consider a process that needs access to three
;; accounts: a3, a4, and a5. Suppose a second process needs access to six
;; accounts: a1, a2, a3, a4, a5, and a6. If accounts are accessed in that order:

;; time ->
;; process 1:       lock a3 -> lock a4 -> lock a5 -> 'done, release all
;; process 2: lock a1 -> lock a2 -> wait for a3 .... lock a3 -> lock a5 -> lock a6

;; Even though process 1 started later and it made process 2 wait, no deadlocks
;; took place.

;; > Rewrite serialized-exchange to incorporate this idea. (You
;; will also need to modify make-account so that each account
;; is created with a number, which can be accessed by sending
;; an appropriate message.)

(require
  "serializer.rkt"
  rackunit)

(define (make-counter)
  (let ((count 0))
    (define (dispatch m)
      (cond ((eq? m 'increase) (set! count (add1 count)))
            ((eq? m 'get) count)
            (else (error "wrong message COUNTER"))))
    dispatch))

(define account-counter (make-counter))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
        (id (begin
              (account-counter 'increase)
              (account-counter 'get))))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) id)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (exchange account1 account2)
 (let ((difference (- (account1 'balance)
                    (account2 'balance))))
  ((account1 'withdraw) difference)
  ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
 (let ((serializer1 (account1 'serializer))
       (serializer2 (account2 'serializer)))
  ((serializer1 (serializer2 exchange))
   account1
   account2)))

(define a1 (make-account-and-serializer 10))
(define a2 (make-account-and-serializer 20))


(check-eq? (a1 'id) 1)
(check-eq? (a2 'id) 2)

(define t1 (thread (lambda () (exchange a1 a2))))
(define t2 (thread (lambda () (exchange a2 a1))))

(thread-wait t1)
(thread-wait t2)

(check-eq? (a1 'balance) 10)
(check-eq? (a2 'balance) 20)
