;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab2 p1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/string)
; string-starts-with?: String String -> Boolean
; Determines whether the first string begins with the second string
(check-expect (string-starts-with? "fundies" "fun") #true)
(check-expect (string-starts-with? "fun" "fundies") #false)
(define (string-starts-with? a b)
  (string-prefix? a b))





