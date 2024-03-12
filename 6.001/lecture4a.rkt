#lang racket

(define (atom? expr)
  (not (pair? expr)))


(define (match pat expr dict)
  (cond ((eq? dict 'failed) 'failed)
        ;; pattern has a tree-like structure
        ;; expr is what we want to:
        ;;  - see if our pattern has the same tree structure
        ;;  - see if matched variables, constants, expressions, have
        ;;      consistent value over the whole tree
        ((atom? pat)
         (if (and (atom? expr) (eq? pat expr))
              dict
              ;; - the expression is not atomic but pattern is
              ;; - the pattern is not exactly the expression 
              ;;    (this is explicit matching, no dict involved)
              'failed))
        ((arbitrary-constant? pat)
         (if (constant? expr)
           (extend-dict pat expr dict)
           'failed))
        ((arbitrary-variable? pat)
         (if (variable? expr)
           (extend-dict pat expr dict)
           'failed))
        ((arbitrary-expression? pat)
           (extend-dict pat expr dict))
        ((atom? expr) 'failed)
        (else (match
                (cdr pat)
                (cdr expr)
                (match (car pat) (car expr) dict)))))

(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ;; colon expression
          ((skeleton-evaluation? s)
           (evaluate (eval-expr s) dict))
          ;; Recursive tree walk
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (evaluate form dict)
  (if (atom? form)
    (lookup form dict)
    (apply
      (eval (lookup (car form) dict)
            user-initial-environment)
      (mapcar (lambda (v) (lookup v dict))
              (cdr form)))))

(define (simplifiers rules)
  (define (simplify-expr expr)
    (try-rules (if (compound? expr)
                 (map simplify-expr expr)
                 expr)))
  (define (try-rules expr)
    (define (scan rls)
      (if (null? rls)
        expr
        (let ((dict
                (match (pattern (car rls))
                  expr
                  (empty-dict))))
          (if (eq? dict 'failed)
            (scan (cdr rls))
            (simplify-expr
              (instantiate
                (skeleton (car rls))
                dict))))))
    (scan rules))
  simplify-expr)

(define (empty-dict) '())

(define (extend-dict pat dat dict)
  (let* ((name (variable-name pat))
         (entry (assq name dict)))
    (cond ((null? entry)
           ;; name not in dict, prepend new entry
           (cons (list name dat) dict))
          ;; name in dict, check that value matches with dat
          ((eq? (cadr entry) dat) dict)
          (else 'failed))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v)
      var
      (cadr v))))
