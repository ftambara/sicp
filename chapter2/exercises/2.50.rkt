#lang racket

(require "2.46.rkt"
         "2.47.rkt"
         "2.49.rkt")


(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; From the book
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ((m (frame-coord-map frame))
           (new-origin (m origin))
           (new-edge1 (sub-vect (m corner1) new-origin))
           (new-edge2 (sub-vect (m corner2) new-origin)))
      (painter (make-frame new-origin new-edge1 new-edge2)))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vector 0.0 1.0); Shift origin to maintain frame pos
                     (make-vector 1.0 1.0); New end of edge1
                     (make-vector 0.0 0.0))); New end of edge2

(define (rotate90 painter)
  (transform-painter painter
                     (make-vector 1.0 0.0)
                     (make-vector 1.0 1.0)
                     (make-vector 0.0 0.0)))
