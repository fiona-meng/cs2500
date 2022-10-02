;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab1-P1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;bingo-word:word-> letter and number
;Given a word
;find the first letter of the word and length
(define (bingo-word word)
  (string-append (substring word 0 1)
                 " "
                 (number->string(string-length word))))

