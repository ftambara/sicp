#lang racket

;; Section 3.2.1

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
