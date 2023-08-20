(display "\n===[ START ]===\n")

(define (echo x)
    (display x)
    (newline)
    x
)

(define (factorial n)
    (define (iter product counter)
        (if (> counter n)
            product
            (iter (* product counter) (+ counter 1))))
    (iter 1 1))

; Exercise 1.9
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (add_rec a b)
    (if (= a 0) b (inc (add_rec
 (dec a) b))))

; (add_rec 4 5)
; (inc (add_rec 3 5))
; (inc (inc (add_rec 2 5)))
; (inc (inc (inc (add_rec 1 5))))
; (inc (inc (inc (inc (add_rec 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(define (add_iter a b)
    (if (= a 0) b (add_iter (dec a) (inc b))))

; (add_iter 4 5)
; (add_iter 3 6)
; (add_iter 2 7)
; (add_iter 1 8)
; (add_iter 0 9)
; 9

; Exercise 1.10
(define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1))))))

; (A 1 10)
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
; (A 0 (A 0 (A 0 (A 0 64))))
; (A 0 (A 0 (A 0 128)))
; (A 0 (A 0 256))
; (A 0 512)
; 1024; 2**10

; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2))))
; (A 1 (A 0 (A 0 4)))
; (A 1 (A 0 8))
; (A 1 16)
; 65536; 2**16

; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; ...; (A 2 2) copied from (A 2 4) chain (3rd line)
; (A 2 4)
; 65536

; (define (f n) (A 0 n))
; Computes 2n

; (define (g n) (A 1 n))
; Computes 2**n

; (define (h n) (A 2 n))
; Computes 2**2**...**2 n times

(define (fib_rec n)
    (if (or (= n 0) (= n 1))
        1
        (+ (fib_rec (- n 1)) (fib_rec (- n 2)))))

(define (fib_iter n)
    (define (iter two_ago one_ago count)
        (if (= count n)
            (+ two_ago one_ago)
            (iter one_ago (+ two_ago one_ago) (+ count 1))))
    (iter 1 0 0))

(define (count_change cents coins)
    ; Count the number of ways to arrive to cents
    ; using from 5 (halfs) down to 1 (pennies) different coins
    (define (coin_value n)
        (cond ((= n 5) 50)
              ((= n 4) 25)
              ((= n 3) 10)
              ((= n 2) 5)
              ((= n 1) 1)
              (else (error "No coin value for n = " n))))
    (cond ((< cents 0) 0)
          ((= cents 0) 1)
          ((= coins 0) 0)
          (else (+ (count_change (- cents (coin_value coins)) coins)
                   (count_change cents (- coins 1))))))

; (define (count_change_iter cents coins)
;     (define (coin_value n)
;         (cond ((= n 5) 50)
;               ((= n 4) 25)
;               ((= n 3) 10)
;               ((= n 2) 5)
;               ((= n 1) 1)
;               (else (error "No coin value for n = " n))))
;     (define (iter combinations coins_left cents_left current_coin)
;         (if (= coins_left 0)
;             combinations
;             (cond 
;                 ((= cents_left 0)
;                     (display "A\n")
;                     (display (list combinations " " coins_left " " cents_left " " current_coin))
;                     (newline)
;                     (if (= current_coin 1)
;                         (iter (+ combinations 1) (- coins_left 1) cents (- coins_left 1))
;                         (iter (+ combinations 1) coins_left cents (- current_coin 1))))
;                 ((< cents_left 0)
;                     (display "B\n")
;                     (display (list combinations " " coins_left " " cents_left " " current_coin))
;                     (newline)
;                     (if (= current_coin 1)
;                         (iter combinations (- coins_left 1) cents (- coins_left 1))
;                         (iter combinations coins_left cents (- current_coin 1))))
;                 (else
;                     (display "C\n")
;                     (display (list combinations " " coins_left " " cents_left " " current_coin))
;                     (newline)
;                     (iter combinations coins_left (- cents_left (coin_value current_coin)) current_coin)))))
;     (iter 0 coins cents coins))
; NOTE: This implementation is wrong. It doesn't any cases mixing the coins.
;   A proper iterative implementation (although it might not be the best) would
;   use a list of coins size to track the number of coins for each state.

; Exercise 1.11
(define (f_rec n)
    (if (< n 3)
        n
        (+ (f_rec (- n 1))
           (* 2 (f_rec (- n 2)))
           (* 3 (f_rec (- n 3))))))

(define (f_iter n)
    (define (iter three_ago two_ago one_ago count)
        (let ((term (if (< count 3)
                        count
                        (+ one_ago (* 2 two_ago) (* 3 three_ago)))))
             (if (= count n)
                 term
                 (iter two_ago one_ago term (+ count 1)))))
    (iter -3 -2 -1 0))

; Exercise 1.12
(define (pascal order position)
    (cond
        ((or (> position (+ order 1)) (< order 0) (< position 1))
            (error "Out of bounds."))
        ((or (= position 1) (= (- position order) 1)) 
            1)
        (else         
            (+ (pascal (- order 1) (- position 1))
               (pascal (- order 1) position)))))

(define (square x) (* x x))

(define (fast_exp base exponent)
    (cond 
        ((= exponent 0) 1)
        ((even? exponent) (fast_exp (square base) (/ exponent 2)))
        (else (* base (fast_exp base (- exponent 1))))))

; Exercise 1.16
(define (iter_exp base exponent)
    (define (iter a base exponent)
        (cond
            ((= exponent 0) a)
            ((even? exponent) (iter a (square base) (/ exponent 2)))
            (else (iter (* a base) base (- exponent 1)))))
    (iter 1 base exponent))

; Exercise 1.17
(define (double x) (* 2 x))
(define (halve x) 
    (if (odd? x)
        (error x " must be even!\n")
        (/ x 2)))

(define (fast_mult_rec a b)
    (cond ((= b 0) 0)
          ((= b 1) a)
          ((even? b) (fast_mult_rec (double a) (halve b)))
          ((odd? b) (+ a (fast_mult_rec a (- b 1))))
          (else (error "Error"))))

; Exercise 1.18
(define (fast_mult a b)
    (define (iter a b invar)
        (cond 
            ((= b 0) invar)
            ((even? b) (iter (double a) (halve b) invar))
            ((odd? b) (iter a (- b 1) (+ invar a)))
            (else (error "Error"))))
    (iter a b 0))

; Exercise 1.19
(define (fib n)
    (define (iter a b p q count)
        (cond ((= count 0) b)
              ((even? count)
               (iter a
                     b 
                     (+ (square p) (square q))
                     (+ (* (square q)) (* 2 p q))
                     (/ count 2)))
              (else (iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
    (iter 1 0 0 1 n))


(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (modulo a b))))

; Exercise 1.20
; Normal order
(gcd 206 
     40)
     (= 40 0); -> #f
 
(gcd 40 
     (modulo 206 40))
    (= (modulo 206 40) 0); -> #f +1

(gcd (modulo 206 40) 
     (modulo 40 (modulo 206 40)))
    (= (modulo 40 (modulo 206 40)) 0); -> #f +2

(gcd (modulo 40 (modulo 206 40))
     (modulo (modulo 206 40) (modulo 40 (modulo 206 40))))
    (= (modulo (modulo 206 40) (modulo 40 (modulo 206 40))) 0); -> #f +4

(gcd (modulo (modulo 206 40) (modulo 40 (modulo 206 40)))
     (modulo (modulo 40 (modulo 206 40)) (modulo (modulo 206 40) (modulo 40 (modulo 206 40)))))
    (= (modulo (modulo 40 (modulo 206 40)) (modulo (modulo 206 40) (modulo 40 (modulo 206 40))))); -> #t +7

(modulo (modulo 206 40) (modulo 40 (modulo 206 40))); -> 2 +4
; total modulo evaluations = 18

; Applicative order
(gcd 206 40)
(gcd 40 (modulo 206 40)); +1
(gcd 40 6)
(gcd 6 (modulo 40 6)); +1
(gcd 6 4)
(gcd 4 (modulo 6 4)); +1
(gcd 4 2)
(gcd 2 (modulo 4 2)); +1
(gcd 2 0)
2
; total modulo evaluations = 4

(define (smallest_divisor num)
    (define (iter count)
        (cond ((> (square count) num) num); No divisor greater than sqrt(num)
              ((= (remainder num count) 0) count)
              (else (iter (+ count 1)))))
    (iter 2))

; xy modulo n = ((x modulo m)*(y modulo m) modulo m)
(define (expmod base exp m)
    (cond
        ((= exp 0) 1)
        ((even? exp) 
         (modulo (square (expmod base (/ exp 2) m)) m))
        (else
         (modulo (* (modulo base m) (expmod base (- exp 1) m)) m))))

(define (randrange min max)
    (+ (random (- max min)) min))

(define (fermats_test num)
    (define (passes? a)
        (= (expmod a num num) a))
    (passes? (randrange 1 num)))

(define (prime? num times)
    (cond ((= times 0) #t)
          ((fermats_test num) (prime? num (- times 1)))
          (else #f)))


(display "===[ END ]===\n")