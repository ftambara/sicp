#lang racket

(require rackunit
         "./3.3.rkt")

(define (make-joint account first-pass second-pass)
  (if (not (valid-pass? account first-pass))
    (error "first-pass is not valid")
    (lambda (password action)
      (if (or (eq? password first-pass)
              (eq? password second-pass))
        (account first-pass action)
        (error "Invalid password")))))

(define (valid-pass? account password)
  (not (exn:fail? (account password 'deposit))))

(let* ((acc (make-account 100 'secret))
      (joint (make-joint acc 'secret 'pass)))
  (check-exn
    exn:fail?
    (lambda () ((joint 'wrong-pass 'withdraw) 30))
    "Incorrect password")
  (check-exn
    exn:fail?
    (lambda () (joint 'secret 'wrong-action) 30)
    "Incorrect action wrong-action")
  (check-eq? ((joint 'secret 'withdraw) 30) 70)
  (check-eq? ((joint 'pass 'withdraw) 30) 40)
  (check-exn
    exn:fail?
    (lambda () (make-joint acc 'wrong-pass 'pass))
    "first-pass is not valid"))
