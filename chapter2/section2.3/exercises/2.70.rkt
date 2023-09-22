#lang racket

(require "2.67.rkt"
         "2.68.rkt"
         "2.69.rkt"
         rackunit)


(define msg
  '('GET 'A 'JOB
         'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
         'GET 'A 'JOB
         'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
         'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP
         'SHA 'BOOM))

(define pairs
  '(('A 2)
    ('GET 2)
    ('SHA 3)
    ('WAH 1)
    ('BOOM 1)
    ('JOB 2)
    ('NA 16)
    ('YIP 9)))

(define code-tree (generate-huffman-tree pairs))

(define encoded-msg (encode msg code-tree))

(integer-list->string encoded-msg)
;; 111111100111101110000000001111111001111011\
;; 100000000011011101010101010101010111011010

;; Check
(check-equal? msg (decode encoded-msg code-tree))

(length encoded-msg)
;; 84

;; Using a fixed-length code, the 8 symbols would require 3 bits
;; to represent them.
;; Using 3 bits per symbol to encode a message of 36 symbols
;; would require 36 * 3, or 108, bits.
;; The variable-length code is 22% shorter.

;; Number of symbols in msg
;;  (length msg)
;;  36
