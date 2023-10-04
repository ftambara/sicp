;; Scheme subset interpreter in Racket
;; Ref.: https://inst.eecs.berkeley.edu/~cs61as/library/scheme1.scm

#lang racket

(require rackunit)

(provide scheme-1)


(define (scheme-1)
  (display "SCHEME-1> ")
  (flush-output)
  (print (eval-1 (read)))
  (newline)
  (scheme-1))

(define (eval-1 expr)
  (cond ((self-evaluating? expr) expr)
        ((symbol? expr) (look-up-global expr))
        ((special-form? expr) (do-special-form expr))
        (else (apply-1 (eval-1 (car expr))
                       (map eval-1 (cdr expr))))))

(define (apply-1 proc args)
  (if (primitive? proc)
      (apply-primitive proc args)
      (eval-1 (substitute (body proc) (formals proc) args))))


(define (self-evaluating? expr)
  (constant? expr))

(define (constant? expr)
  (or (number? expr)
      (string? expr)
      (boolean? expr)
      (procedure? expr)))

(define (look-up-global expr)
  (eval expr))

(define (special-form? expr)
  (or (quote-expr? expr) (if-expr? expr) (lambda-expr? expr)))

(define (expr-checker type)
  (lambda (expr) (and (pair? expr) (eq? (car expr) type))))

(define (quote-expr? expr)
  ((expr-checker 'quote) expr))

(define (if-expr? expr)
  ((expr-checker 'if) expr))

(define (lambda-expr? expr)
  ((expr-checker 'lambda) expr))

(define (do-special-form expr)
  (cond ((quote-expr? expr) (cadr expr))
        ((if-expr? expr)
         (if (eval-1 (cadr expr))
             (eval-1 (caddr expr))
             (eval-1 (cadddr expr))))
        ((lambda-expr? expr) expr)
        (else (error "Unrecognized special form" expr))))

(define (apply-primitive proc args)
  (apply proc args))

(define (substitute exp params args)
  (define (lookup name params args)
    (cond ((null? params) name)
          ((eq? name (car params)) (maybe-quote (car args)))
          (else (lookup name (cdr params) (cdr args)))))

  (define (maybe-quote value)
    (cond ((lambda-expr? value) value)
          ((constant? value) value)
          ((procedure? value) value)	; real Scheme primitive procedure
          (else (list 'quote value))))
  (define (sub-rec exp params args bound)
    (cond ((constant? exp) exp)
          ((symbol? exp)
           (if (memq exp bound)
               exp
               (lookup exp params args)))
          ((quote-expr? exp) exp)
          ((lambda-expr? exp)
           (list 'lambda
                 (cadr exp)
                 (sub-rec (caddr exp) params args (append bound (cadr exp)))))
          (else (map (lambda (subexp) (sub-rec subexp params args bound))
                     exp))))
  (sub-rec exp params args '()))

(define (body proc)
  (caddr proc))

(define (formals proc)
  (cadr proc))
