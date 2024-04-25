#lang racket

(require rackunit)

(provide make-account)

(define (make-account balance password)
  (define (protect-by-pass f)
    (lambda (pass-attempt arg)
      (if (not (eq? pass-attempt password))
        (error "Incorrect password")
        (f arg))))
  (define (account action)
    (cond ((eq? action 'withdraw)
           (lambda (amount)
             (if (< balance amount)
               (error ("Not enough funds"))
               (begin
                 (set! balance (- balance amount))
                 balance))))
          ((eq? action 'deposit)
           (lambda (amount)
             (set! balance (+ balance amount))
             balance))
          (else
            (error "Incorrect action" action))))
  (protect-by-pass account))


(let ((acc (make-account 100 'secret)))
  (check-exn
    exn:fail?
    (lambda () ((acc 'wrong-pass 'withdraw) 30))
    "Incorrect password")
  (check-exn
    exn:fail?
    (lambda () (acc 'secret 'wrong-action) 30)
    "Incorrect action wrong-action")
  (check-eq? ((acc 'secret 'withdraw) 30) 70)
  (check-eq? ((acc 'secret 'withdraw) 30) 40)
  (check-eq? ((acc 'secret 'deposit) 10) 50))
