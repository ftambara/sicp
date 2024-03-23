#lang racket

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))


;;  ┌────────────────────┐
;;  │ GLOBAL ENVIRONMENT │
;;  │                    │
;;  │ make-withdraw      │
;;  │ W2                 │
;;  │ W1                 │
;;  └────────────────────┘
;;        ^     ^   
;;        ┆     └╴╴╴╴╴╴╴╴╴╴╴┐
;;        ┆                 ┆
;;        ┆         ┌─FRAME I──────────────┐
;;        ┆         │ initial-amount = 100 │
;;        ┆         └──────────────────────┘  There is an additional
;;        ┆                 ┆                 procedure object created
;;  make-withdraw   ┌─FRAME II──────┐         that creates FRAME I
;;        ┆         │ balance = 1̶0̶0̶ │╴╴╴┐     with params: balance
;;    ╱╲  ╱╲        └───────────────┘   ┆
;;   ╱  ╲╱  ╲           W1    ┆  50     ┆
;;   ╲  ╱╲  ╱            ╱╲  ╱╲         ┆
;;    ╲╱  ╲╱            ╱  ╲╱  ╲        ┆
;;    |                 ╲  ╱╲  ╱        ┆
;;    V                  ╲╱  ╲╱         ┆
;;  params: initial-amount              ┆
;;  (let ((balance ...)) |              ┆
;;      (...))           v              ┆
;;                   params: amount     ┆
;;                   (if (>= balance amount)
;;                     (...))           ┆
;;                                      ┆
;;        ┌─FRAME III───┐               ┆
;;        │ amount = 50 │╴╴╴╴╴╴╴╴╴╴╴╴╴╴╴┘
;;        └─────────────┘
;;          Change balance in FRAME II
;;          and return new balance (50)

;; The main difference with the let-less version is that
;; there are more frames and procedure objects created.

;; Edit:
;; In the original version, there were more procedure objects than
;; there were lambda expressions in the code. That cannot be, as
;; each procedure object is only created by evaluating a lambda
;; expression.
