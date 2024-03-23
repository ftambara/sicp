#lang racket

;; “The desire to model systems composed of objects that have changing state
;; leads us to the need to modify compound data objects, as well as to
;; construct and select from them.”
;; I don't see how we jumped to compound data objects from that argument.

;; Mutable here probably refers to something slightly different that what I'm
;; used to from other languages. In this context, any bound variable that
;; has a procedure that can change its value is mutable. This is different
;; from my understanding of mutable in other languages, where it refers to
;; types that allow their values to be changed. I wouldn't have considered
;; instance variables to be mutable, or merely reassigning a variable to a
;; new value to be mutation.
;; These two views can be reconciled by considering that in other languages,
;; variables are mutable, but the values they reference may not be (for
;; example, integer values are immutable, while array values are mutable, even
;; if we don't affect the variable's reference).
