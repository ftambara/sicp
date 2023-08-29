#lang racket


(define l1 '(1 3 (5 7) 9))
(car                        ; => 7
    (cdr                    ; => (7)
        (car                ; => (5 7)
            (cdr            ; => ((5 7) 9)
                (cdr l1))))); => (3 (5 7) 9)
(define l2 '((7)))
(car (car l2))

(define l3 '(1 (2 (3 (4 (5 (6 7)))))))
(cadr                             ; => 7
    (cadr                         ; => (6 7)
        (cadr                     ; => (5 (6 7))
            (cadr                 ; => (4 (5 (6 7)))
                (cadr             ; => (3 (4 (5 (6 7))))
                    (cadr l3)))))); => (2 (3 (4 (5 (6 7)))))
