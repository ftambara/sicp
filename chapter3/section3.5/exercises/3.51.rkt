#lang racket

(require "../../modules/streams.rkt")

(define (show x) (displayln x) x)

(displayln "Tag 1")
(define x
  (stream-map-book show
              (stream-enumerate-interval 0 10)))
#|
Prints:
0
x = (cons 0 (delay (stream-enumerate-interval 1 10)))
|#

(displayln "Tag 2")
(stream-ref-book x 5)
#|
(cons 0
      (show 1)
      (delay (stream-enumerate-interval 2 10)))
...
(list 0
      1
      2
      3
      4
      (show 5)
      (delay (stream-enumerate-interval 6 10)))
Prints:
1
2
3
4
5
Returns 5
|#

(displayln "Tag 3")
(stream-ref-book x 7)
#|
(cons 0
      (delay (stream-enumerate-interval 1 10)))
...
(list 0
      1
      2
      3
      4
      5
      (show 6)
      (show 7)
      (delay (stream-enumerate-interval 8 10)))

Prints:
6
7
Returns 7
|#

;; Note: I had to use custom map and ref versions, which are like
;; Racket's standard versions but evaluate the stream car eagerly
