#lang racket

(require rackunit)


;; From the book (section 2.4)
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; To be defined
(define get-coercion void)
(define get void)

;; Solution
(define (unique lst)
  (define (iter accum rest)
    (if (empty? rest)
        accum
        (if (member (car rest) accum)
            (iter accum (cdr rest))
            (iter (cons (car rest) accum) (cdr rest)))))
  (iter '() lst))

(check-equal? (unique '(1 2 3 2 4 3))
              '(4 3 2 1))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (find-coerced-proc op args)))
    (if proc
        (apply proc (map contents args))
        (error "No method for these types" op type-tags))))

;; Untested and complicated
(define (find-coerced-proc op args)
  (define (rec types-from)
    (if (empty? types-from)
        null
        (let* ((types-to (map type-tag args))
               (coercions (filter-map (lambda (coercion) coercion)
                                      (lambda (type-to)
                                        (get-coercion (car types-from)
                                                      type-to))
                                      types-to)))
          (if (< (length coercions) (length args))
              (rec (cdr types-from))
              (let ((coerced-args
                     (map (lambda (arg) ((get-coercion (car types-from)
                                                       (type-tag arg))
                                         arg)
                            args))))
                (let ((proc (get op (map type-tag coerced-args))))
                  (if proc proc null)))))))

  (rec (unique (map type-tag args))))


;; Since not all combinations are tested, this apply-generic definition will
;; not find procedures for different mixed-type definitions of an operation.
;; For example, (oper type1 type2 type3) might be defined for
;; (oper type1 type1 type2), but our procedure will not try any combinations
;; besides same-type and the initial case.
