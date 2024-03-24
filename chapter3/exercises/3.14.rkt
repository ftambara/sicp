#lang racket

(require rackunit
         "3.12.rkt")

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (mcdr x)))
        (set-mcdr! x y)
        (loop temp x))))
  (loop x '()))

;; mystery reverses the list x in place

(define v (mlist 'a 'b 'c 'd))

;; v -> [ ● | ●-]-->[ ● | ● ]-->[ ● | ● ]-->[ ● | / ]
;;        |           |           |           |
;;        v           v           v           v
;;        'a          'b          'c          'd

((lambda ()
   (check-equal? v (mlist 'a 'b 'c 'd))
   (define mist (mystery v))
   (check-equal? mist (mlist 'd 'c 'b 'a))
   (check-equal? v (mlist 'a))))

;; (loop v '())

;; temp -> [ ● | ●-]-->[ ● | ● ]-->[ ● | / ]
;;           |           |           |
;;           v           v           v
;;           'b          'c          'd
;;
;; x -> [ ● | / ]
;;        |
;;        v
;;        'a

;; (loop temp x)

;; temp -> [ ● | ●-]-->[ ● | / ]
;;           |           |
;;           v           v
;;           'c          'd
;;
;; x -> [ ● | ● ]-->[ ● | / ]
;;        |           |
;;        v           v
;;        'b          'a

;; (loop temp x)

;; temp -> [ ● | / ]
;;           |
;;           v
;;           'd
;;
;; x -> [ ● | ● ]-->[ ● | ● ]-->[ ● | / ]
;;        |           |           |
;;        v           v           v
;;        'c          'b          'a

;; (loop temp x)

;; temp -> nil
;;
;; w,x -> [ ● | ● ]-->[ ● | ● ]-->[ ● | ● ]-->[ ● | / ]
;;          |           |           |           |
;;          v           v           v           v
;;          'd          'c          'b          'a

;; w points to the reversed list
;; v is only mutated on the first iteration, since v points to the first pair
;; of the original list.
;; after running (mystery v), then, v points to the last pair of the reversed list.
