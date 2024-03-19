#lang racket

(require rackunit)

(define (make-account balance password)
  (define (protect-by-pass f max-attempts)
    (define incorrect-attempts 0)
    (lambda (pass-attempt arg)
      (if (not (eq? pass-attempt password))
        (if (>= incorrect-attempts max-attempts)
          (call-the-cops)
          (begin
            (set! incorrect-attempts (+ incorrect-attempts 1))
            (error "Incorrect password")))
        (begin
          (set! incorrect-attempts 0)
          (f arg)))))

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
  (protect-by-pass account 7))

(define (call-the-cops)
  (error "https://www.youtube.com/watch?v=20mBi8-QsSc"))


(let ((acc (make-account 100 'secret)))
  (check-exn 
    exn:fail?
    (lambda () ((acc 'wrong-pass 'withdraw) 30))
    "Incorrect password")
  (check-exn 
    exn:fail?
    (lambda () ((acc 'wrong-pass 'withdraw) 30))
    "Incorrect password")
  (check-exn 
    exn:fail?
    (lambda () ((acc 'wrong-pass 'withdraw) 30))
    "Incorrect password")
  (check-exn 
    exn:fail?
    (lambda () ((acc 'wrong-pass 'withdraw) 30))
    "Incorrect password")
  (check-exn 
    exn:fail?
    (lambda () ((acc 'wrong-pass 'withdraw) 30))
    "Incorrect password")
  (check-exn 
    exn:fail?
    (lambda () ((acc 'wrong-pass 'withdraw) 30))
    "Incorrect password")
  (check-exn 
    exn:fail?
    (lambda () ((acc 'wrong-pass 'withdraw) 30))
    "https://www.youtube.com/watch?v=20mBi8-QsSc"))
