;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1review-p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 1 Review, Problem 3 ==

; Consider the following data definition:

(define-struct traincar [name c others])

; A Train is a (make-traincar String String Train)
; Interpretation: a train
; - name is the name of the current train car
; - c is the color of the current train car
; - others is the other train cars it is attached to

; (a) What is the problem with this data definition? Describe your answer in a comment.
; Hint: try defining examples of a Train



; (b) Consider the following function:

; foo: String Boolean Number->Number
; confuses students ;)

(check-expect (foo "hello" #true 100) 100)
(check-expect (foo "hi" #false 2) 10)

(define (foo a b c)
  (if b
      c
      (max
       (string-length
        (string-append (replicate c a) " + " a))
       (* (- c) -5))))

; Replace "SIGNATURE HERE" with a signature for the function foo
; (since this code is nonsense, do not provide a purpose statement)

; Now confirm your signature by replacing "TESTS HERE" with a
; comprehensive set of tests.

