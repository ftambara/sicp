#lang racket

(require "../../section2-4/notes.rkt")


(define (put op types fn) void)

(define (get op types) void)

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
               (list op type-tags)))))

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

;; The call to the generic procedure magnitude
;; traces the following calls:

(define z '(list 'complex 'rectangular (cons 3 4)))

(magnitude z)
(apply-generic 'magnitude z)
;; => type-tags = '(complex)
;; => proc = (get 'magnitude '(complex))
;;           magnitude
;; (apply magnitude (list 'rectangular (cons 3 4)))

(apply-generic 'magnitude (list 'rectangular (cons 3 4)))
;; => type-tags = '(rectangular)
;; => proc = (get 'magnitude '(rectangular))
;;           magnitude-rectangular
;; (apply magnitude-rectangular (cons 3 4))

;; (magnitude-rectangular (cons 3 4))
;; => 5

;; There is one call for each type level (not for each argument).
