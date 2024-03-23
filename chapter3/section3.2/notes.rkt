#lang racket

;; Section 3.2

;; # Constructing procedures
;;  A new process object is created. The lambda expression is evaluated
;;  and it's code text stored as the process object body. A pointer to the
;;  environment where the lambda expression was evaluated in is stored in
;;  the object.
;;
;; # Applying procedures
;;  A new environment is constructed, consiting of a new frame that
;;  binds the arguments of the call, and enclosed by the process' object
;;  environment. The process' body is evaluated in the context of the
;;  newly generated environment.
;;
;; # What define does
;;  define expressions associate a symbol with a value, in the
;;  environment where it's evaluated.
;;
;; # What set! does
;;  set! expressions find the firt frame of the environment that contain
;;  the given variable name, and modify the saved value for the new one.

;; Procedure objects are created in one way only: by evaluating a λ-expression.
;; (define (f x) <expr>) is syntactic sugar for (define f (λ (x) <expr>))

;; The rest of the section are applied evaluations of the above concepts on
;; increasingly complex examples involving local state and definitions.

;; I had some trouble understanding which of the stated rules spcified that
;; evaluating code that consists of another λ-expression should create the
;; new procedure object in the context of the new environment (the one that
;; binds the arguments of the enclosing λ-expression arguments), and not in
;; the context of the the enclosing λ-expression environment itself.
;; As I understand it now, it's defined in the application rule, the body of
;; the procedure is evaluated in the context of the new environment.

;; The key was understanding that there must be a one-to-one relation of
;; procedure objects to lambda expressions. Without having this rule, the
;; more intricate exambles involving `let`s (i.e exercise 3.10) got quite
;; confusing.
