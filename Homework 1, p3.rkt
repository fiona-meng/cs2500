;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 1, p3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 1, Problem 3 ==



; TODO: Define below the function salutation, which takes a first name, a last name,
; and a greeting word, and then produces a resulting combined welcome.
;
; Be careful to correct for capitalization in all inputs: no matter how
; they are supplied, all should be output with the first letter upper-case
; and the rest lower-case. For example...
;
; (salutation "hello" "jane" "doe") should result in "Hello Jane Doe"
; (salutation "WELCOME" "bOb" "SMiTh") should result in "Welcome Bob Smith"
;
; Hint: if you are doing the same process over and over... maybe that should
; be its own function?
; 
; You should include a signature and purpose for all functions you write.
; You can assume that all inputs will be a single word of at least one
; character.

;salutation: String String String -> String
;the function salutation produces a combined welcome by adding first name, last name, and a greeting word together
(check-expect (salutation "hello" "jane" "doe") "Hello Jane Doe")
(check-expect (salutation "WELCOME" "bOb" "SMiTh") "Welcome Bob Smith")


(define (string-titlecase word)
  (string-append (string-upcase (substring word 0 1)) (string-downcase (substring word 1))))
   
(define (salutation a b c)
  (string-append (string-titlecase a) " " (string-titlecase b) " " (string-titlecase c)))
