#lang racket

(require
  "2.46.rkt"
  "2.47.rkt"
  "2.48.rkt")


;; From the book
(define (frame-coord-map frame)
  (lambda (frame-vec)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect frame-vec) (edge1-frame frame))
               (scale-vect (ycor-vect frame-vec) (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

;; Solutions
(define outline-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0 1) (make-vect 1 1))
    (make-segment (make-vect 1 1) (make-vect 1 0))
    (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define x-painter
  (segments->painter
   (list
    (make-segment (make-vector 1 0) (make-vector 0 1))
    (make-segment (make-vector 0 0) (make-vector 1 1)))))

(define diamond-painter
  (segments->painter
   (list
    (make-segment (make-vector 0 .5) (make-vector .5 1))
    (make-segment (make-vector .5 1) (make-vector 1 .5))
    (make-segment (make-vector 1 .5) (make-vector .5 0))
    (make-segment (make-vector .5 0) (make-vector 0 .5)))))

; I've let Github's Copilot decide here
(define wave-painter
  (segments->painter
   (list
    (make-segment (make-vector 0 .5) (make-vector .25 1))
    (make-segment (make-vector .25 1) (make-vector .5 .5))
    (make-segment (make-vector .5 .5) (make-vector .75 1))
    (make-segment (make-vector .75 1) (make-vector 1 .5))
    (make-segment (make-vector 1 .5) (make-vector 1 0))
    (make-segment (make-vector 1 0) (make-vector 0 0)))))


;; To be defined later
(define (draw-line start-vector end-vector)
  (void))
