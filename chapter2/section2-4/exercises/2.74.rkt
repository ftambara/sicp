#lang racket



(define (get op type) (display "Not implemented yet"))
(define (put op type fn) (display "Not implemented yet"))

;; Do this for each division
(define (install-division-a-package)
  ;; a.
  (define (get-record employee-name personnel-file)
    ;; BODY
    void)

  ;; b.
  (define (get-salary employee-record)
    ;; BODY
    void)

  ;; a.
  (put 'get-record 'division-a get-record)
  ;; b.
  (put 'get-salary 'division-a get-salary)

  'done)

;; a.

;; Generic procedure
(define (get-record employee-name division-id)
  ((get 'get-record division-id) employee-name))

;; The division files' only requirement is that they be keyed on
;; employees' names.
;; Typing information is given by the division identifier, which
;; could be extracted either from an agreed-upon location in the
;; personnel file, or otherwise manually


;; b.

;; Generic procedure
(define (get-salary employee-record division-id)
  ((get 'get-salary division-id) employee-record))

;; There are no requirements on the record structure other than that
;; it should contain salary information.
;; Typing information is again given by the division identifier.


;; c.

(define (find-employee-record employee-name division-files)
  (if (null? division-files)
      false
      (let ((record (get-record employee-name (car division-files))))
        (if record
            record
            (find-employee-record employee-name (cdr division-files))))))

;; d.
;; It should define another install-division-x-package procedure,
;; with the analogous implementations for that new division.
