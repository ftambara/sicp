#lang racket

(define x (list 'a 'b))
(define z1 (cons x x))

;; z1 ──>[ ● │ ● ]
;;         │   │
;;    ┌────┴───┘
;;    │ 
;; x ─┴>[ ● │ ●─]──>[ ● │ / ]
;;        │           │
;;        v           v
;;        'a          'b

(define z2 (cons (list 'a 'b) (list 'a 'b)))

;; z2 ──>[ ● │ ● ]
;;         │   │
;;   ┌─────┘   │
;;   │  ┌──────┘
;;   │  └>[ ● │ ●─]──>[ ● │ / ]
;;   │      │           │
;;   │      v           v
;;   │      'a          'b
;;   │      ^           ^
;;   │      │           │
;;   └───>[ ● │ ●─]──>[ ● │ / ]

(define set─car! void)  ;; Not defined in Racket
(define (set─to─wow! x) (set─car! (car x) 'wow) x)

(set─to─wow! z1)

;; z1 ──>[ ● │ ● ]
;;         │   │
;;    ┌────┴───┘
;;    │ 
;; x ─┴>[ ● │ ●─]──>[ ● │ / ]
;;        │           │
;;        v           v
;;        'wow        'b

(set─to─wow! z2)

;; z2 ──>[ ● │ ● ]
;;         │   │
;;   ┌─────┘   │
;;   │  ┌──────┘
;;   │  └>[ ● │ ●─]──>[ ● │ / ]
;;   │      │           │
;;   │      v           v
;;   │      'a          'b
;;   │                  ^
;;   │                  │
;;   └───>[ ● │ ●─]──>[ ● │ / ]
;;          │
;;          v
;;          'wow
