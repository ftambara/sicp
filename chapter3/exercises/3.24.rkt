#lang racket

(require
  compatibility/mlist
  rackunit)

(define (assocfn fn key mlst)
  (cond ((empty? mlst) false)
        ((fn key (mcar (mcar mlst))) (mcar mlst))
        (else (assocfn fn key (mcdr mlst)))))

(define (make-table same-key?)
  (let ((table (mcons '*table* null)))
    (define (lookup key1 key2)
      (let ((subtable (assocfn same-key? key1 (mcdr table))))
        (if subtable
          (let ((record (assocfn same-key? key2 (mcdr subtable))))
            (if record
              (mcdr record)
              false))
          false)))

    (define (insert! key1 key2 value)
      (let ((subtable (assocfn same-key? key1 (mcdr table))))
        (if subtable
          (let ((record (assocfn same-key? key2 (mcdr subtable))))
            (if record
              (set-mcdr! record value)
              ;; new sub-level key
              (set-mcdr! subtable (mcons (mcons key2 value) (mcdr subtable)))))
          ;; new top level key
          (set-mcdr! table
                     (mcons (mlist key1 (mcons key2 value))
                            (mcdr table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "invalid method" m))))

    dispatch))

;; Tests
(define table (make-table eq?))
(check-equal? ((table 'insert!) 'a 'b 1) 'ok)
(check-equal? ((table 'lookup) 'a 'b) 1)
(check-false ((table 'lookup) 'a 'c))
(check-equal? ((table 'insert!) 'a 'c 2) 'ok)
(check-equal? ((table 'lookup) 'a 'b) 1)
(check-equal? ((table 'lookup) 'a 'c) 2)
(check-false ((table 'lookup) 'b 'c))
(check-equal? ((table 'insert!) 'b 'c 3) 'ok)
(check-equal? ((table 'lookup) 'b 'c) 3)

;; Test with different key comparison function
(define (in-range? num-or-range range)
  ;; Check if num-or-range is contained in range
  (cond ((pair? num-or-range)
         (and (>= (car num-or-range) (car range))
              (<= (cdr num-or-range) (cdr range))))
        (else (and (>= num-or-range (car range))
                   (< num-or-range (cdr range))))))

(check-true (in-range? 1 (cons 1 5)))
(check-true (in-range? (cons 1 5) (cons 1 5)))

(define range-table (make-table in-range?))
(check-equal? ((range-table 'insert!) (cons 1 5) (cons 1 3) 'a) 'ok)
(check-equal? ((range-table 'lookup) 1 2) 'a)
(check-equal? ((range-table 'insert!) (cons 1 5) (cons 3 7) 'b) 'ok)
(check-equal? ((range-table 'lookup) 1 2) 'a)
(check-equal? ((range-table 'lookup) 3 5) 'b)
(check-false ((range-table 'lookup) 3 11))
(check-false ((range-table 'lookup) 12 1))
