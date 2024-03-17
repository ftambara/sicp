#lang racket

;; State is the necessary information about past actions to determine behavior.
;; Objects help us model a system. To work effectively, an object or a set of
;; objects must have little coupling to other parts of the system. This depends
;; not only on the chosen design, but on the nature of the system itself.

(require rackunit)

;; Section 3.1.1

(define withdraw
  ;; If let's balance declaration is evaluated once, why is set! needed?
  (let ((balance 100))
    (lambda (amount)
      (cond ((< balance amount)
             (error "Insufficient funds"))
            (else
              (set! balance (- balance amount))
              balance)))))

(check-eq? (withdraw 10) 90)
(check-eq? (withdraw 10) 80)
(check-eq? (withdraw 60) 20)
(check-exn exn:fail? (lambda () (withdraw 50)))

;; No need to use let, balance is already bound,
;; and present in the local environment
(define (make-withdraw balance)
  (lambda (amount)
    (if (< balance amount)
      (error "Insufficient funds")
      (begin
        (set! balance (- balance amount))
        balance))))

;; Testing inside of a lambda just as a test
((lambda (withdraw-fn) 
   (begin 
     (check-eq? (withdraw-fn 10) 90)
     (check-eq? (withdraw-fn 10) 80)
     (check-eq? (withdraw-fn 60) 20)
     (check-exn exn:fail? (lambda () (withdraw-fn 50)))))
 (make-withdraw 100))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

(let ((account (make-account 100)))
   (begin
     (check-eq? ((account 'withdraw) 10) 90)
     (check-eq? ((account 'deposit) 20) 110)))

;; Section 3.1.2

;; Why use objects?
;; If we knew every minute physical property of the action of throwing a coin,
;; we could predict which side it would land on. Since we don't, we simply
;; understand the coin toss as a complex system and observe the result, instead
;; of computing it ourselves.
;; In the same way, to one part of the system, other complex parts appear to
;; change with time. If we wanted to use a function instead, we would have to
;; know and keep track of every parameter the system needs, and interaction
;; would leak other processes' internals into every part of our system.
;; Everything would be related, there would be no system-level abstraction.

;; Section 3.1.3

(define (factorial n)
  (let ((total 1))
    (define (iter)
      (if (or (= n 0) (= n 1))
        total
        (begin (set! total (* total n))
               (set! n (- n 1))
               (iter))))
    (iter)))

(factorial 5)
