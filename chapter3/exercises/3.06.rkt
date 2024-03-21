#lang racket

(require rackunit)

(define (gen-random x)
  (random-seed x)
  (random (expt 2 16)))

(define rand
  (let ((seed (current-seconds)))
    (lambda (action)
      (cond 
         ((eq? action 'generate)
          (set! seed (gen-random seed))
          seed)
         ((eq? action 'reset)
          (lambda (new-seed)
            (set! seed new-seed)
            seed))
         (else
           (error "Invalid adction" action))))))

(check-eq? (gen-random 0) (gen-random 0))

(rand 'generate)
(rand 'generate)
(rand 'generate)

(display "Resetting seed to 123\n")
((rand 'reset) 123)
(rand 'generate)
(rand 'generate)

(display "Resetting seed to 123\n")
((rand 'reset) 123)
(rand 'generate)
(rand 'generate)

(display "Resetting seed to 456\n")
((rand 'reset) 456)
(rand 'generate)
(rand 'generate)
