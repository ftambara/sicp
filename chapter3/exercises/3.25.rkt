#lang racket

(require rackunit)

(define (assoc key records)
  (cond ((null? records) false)
        ((eq? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (make-table)
  (let ((table (mcons '*table* null)))
    (define (lookup keys)
      (define (lookup-iter subtable rest-keys)
        (let ((next (assoc (car rest-keys) (mcdr subtable))))
          (if next
            (if (null? (cdr rest-keys))
              (mcdr next)
              (lookup-iter next (cdr rest-keys)))
            false)))
      (lookup-iter table keys))

    (define (insert! keys value)
      (define (create-subtables keys value tail)
        (if (null? keys)
          value
          (mcons (mcons (car keys)
                        (create-subtables (cdr keys) value tail))
                 tail)))

      (define (insert-iter subtable rest-keys)
        (let ((next (assoc (car rest-keys) (mcdr subtable))))
          (if next
            (cond ((not (pair? (mcdr next)))
                   ;; If next is a record instead of a subtable, we need to
                   ;; discard its value and replace it with a new subtable.
                   (set-mcdr! next (create-subtables rest-keys value null)))
                  ((null? (cdr rest-keys))
                   ;; If this is the last key, update the value (this
                   ;; overwrites the previous value/subtable).
                   (set-mcdr! next value))
                  (else (insert-iter next (cdr rest-keys))))
            (set-mcdr! subtable
                       (create-subtables rest-keys value (mcdr subtable))))))

      (insert-iter table keys)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "invalid method" m))))

    dispatch))

;; Tests
(define t (make-table))
(check-equal? ((t 'insert!) '(a b c) 1) 'ok)
(check-equal? ((t 'lookup) '(a b c)) 1)

(check-equal? ((t 'insert!) '(a b d) 2) 'ok)
(check-equal? ((t 'lookup) '(a b c)) 1)
(check-equal? ((t 'lookup) '(a b d)) 2)

(check-equal? ((t 'insert!) '(a b x y z) 10) 'ok)
(check-equal? ((t 'lookup) '(a b c)) 1)
(check-equal? ((t 'lookup) '(a b d)) 2)
(check-equal? ((t 'lookup) '(a b x y z)) 10)

(check-not-equal? ((t 'lookup) '(a b x y)) false)
