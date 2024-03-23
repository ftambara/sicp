#lang racket

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
          (else
            (error "Unknown request: MAKE-ACCOUNT"
                   m))))
  dispatch)

(define acc (make-account 50))
((acc 'deposit) 40)
;; 90
((acc 'withdraw) 60)
;; 30


;; GLOBAL
;; #<procedure:make-account>

;; (define acc (make-account 50))
;; FRAME 1 -> GLOBAL
;; balance = 50
;; withdraw = #<procedure:withdraw>
;; deposit = #<procedure:deposit>
;; dispatch = #<procedure:dispatch>
;; RETURN #<procedure:dispatch>
;;
;; ((acc 'deposit) 40)
;; FRAME 2 -> FRAME 1
;; m = 'deposit
;;
;; FRAME 3 -> FRAME 1
;; amount = 40
;; RETURN 90
;;
;; ((acc 'withdraw) 60)
;; FRAME 4 -> FRAME 1
;; m = 'withdraw
;;
;; FRAME 5 -> FRAME 1
;; amount = 60
;; RETURN 30

;; Local state is kept in FRAME 1's balance variable
;; A second account created with make-account would have
;; its own frame, with its own balance variable.
;; The only shared parts may be the code of the procedure
;; objects, depending on the implementation of the language.
