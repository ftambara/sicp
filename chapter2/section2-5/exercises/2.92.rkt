#lang racket


;; I'm not going to solve this, it's too much for too little and I'd prefer
;; to get on with the book.
;; However, here are some thoughts I had while trying to solve it.
;;
;; To add or multiply polynomials in different variables, we must:
;;  - Choose a dominant variable in which the result will be expressed
;;  - If the polinomials have different variables, convert the infringent
;;      one into a polynomial with the variable of the other. This is the
;;      hard part
;; 
;; To convert a polynomial into other of a different variable:
;;  - Each term has a coefficient might itself be a polynomial.
;;  - If it isn't, we treat it as a term in the new variable with order 0
;;  - If it is a polynomial, but has a different variable, we treat it as
;;      a term in the new variable with order 0 and move on with life[^1].
;;  - If it's a polynomial in the variable we are looking for, we still need to
;;      multiply the old variable expressed as a polynomial in a different
;;      variable with the new coefficient.
;;      
;; [^1] You wish. Instead we should recursively check and convert until we
;;      reach a non polynomial coefficient. I haven't remotely thought about
;;      how to solve that problem.

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1) (term-list p2)))
    (add-poly p1 (convert-poly (variable p1) p2))))

(define (convert-poly var poly)
  ;; Mistery
  void)
