#lang racket

(require rackunit)


(car ''abracadabra)
; To ease understanding, expand quote abbreviation
; into long form.
; (car (quote (quote abracadabra)))

; The order of evaluation for quote seems to be
; normal order.
; (car '(quote abracadabra))
; => quote

(check-equal? (car ''abracadabra) (car (quote (quote abracadabra))))