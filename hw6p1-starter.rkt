;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 6, Problem 1 ==

; Consider the following functions:


; good-job : [List-of NonNegReal] -> [List-of NonNegReal]
; adds 20% to all supplied costs

(check-expect
 (good-job '())
 '())

(check-expect
 (good-job
  (cons 10 (cons 5 '())))
 (cons 12 (cons 6 '())))

(define (good-job lon)
  (new-function lon '() cons add-thanks))


; add-thanks : NonNegReal -> NonNegReal
; adds 20% to the supplied cost

(check-expect (add-thanks 10) 12)
(check-expect (add-thanks 5) 6)

(define (add-thanks cost)
  (* cost 1.2))


; total-length : [List-of String] -> Nat
; returns the total length of all strings in the list

(check-expect
 (total-length
  '())
 0)

(check-expect
 (total-length
  (cons "a" (cons "bb" '())))
 3)

(define (total-length los)
  (new-function los 0 + string-length))

; new-function: (X Y Z)[list-of X][X->Y][Y->Z]->Z
; to abstract total-length and good-job function
(check-expect (new-function (cons 1 (cons 2 (cons 3'()))) 0 + *)6)
(check-expect (new-function (cons 102 (cons 77 (cons 82'()))) 0 + *)261)              
(define (new-function lon base a f)
  (cond
    [(empty? lon) base ]
    [(cons? lon)
     (a
      (f (first lon))
      (new-function (rest lon)base a f))]))

; TODO #1: abstract good-job and total-length.
; Be sure to re-define the functions using your new
; abstraction.


; TODO #2: use your new function to design the function
; acronym-image, which takes in a list of strings and
; visualizes them vertically stacked, with the first
; letter bolded (to highlight the acronym). The above/align
; function will be quite useful, as will the "modern" font
; (which is fixed-width, so all letters line up nicely).
; You can assume all supplied strings will have at least
; two letters.

; acronym-image: [list-of String]->Image
; visualizes a list of word vertically
(check-expect (acronym-image (cons "World" (cons "Help" '())))
              (above/align
               "left" (beside
                       (text/font (substring "World" 0 1)
                                  24 "black" "Gill Sans" 'modern 'normal 'bold #f)
                       (text (substring "World" 1) 24 "black"))
               (beside
                (text/font (substring "Help" 0 1)
                           24 "black" "Gill Sans" 'modern 'normal 'bold #f)
                (text (substring "Help" 1) 24 "black"))
               (text " " 10 "black")))
              
(define (acronym-image los)
  (new-function los (text " " 10 "black") above/align-helper find-acronym))

; above/align-helper: Image->Image
; makes image above each other
(check-expect (above/align-helper (beside
                                   (text/font (substring "World" 0 1)
                                              24 "black" "Gill Sans" 'modern 'normal 'bold #f)
                                   (text (substring "World" 1) 24 "black")) 
                                  (beside
                                   (text/font (substring "Help" 0 1)
                                              24 "black" "Gill Sans" 'modern 'normal 'bold #f)
                                   (text (substring "Help" 1) 24 "black")))
              (above/align
               "left" (beside
                       (text/font (substring "World" 0 1)
                                  24 "black" "Gill Sans" 'modern 'normal 'bold #f)
                       (text (substring "World" 1) 24 "black"))
               (beside
                (text/font (substring "Help" 0 1)
                           24 "black" "Gill Sans" 'modern 'normal 'bold #f)
                (text (substring "Help" 1) 24 "black"))))      
(define (above/align-helper a b)
  (above/align "left" a b))

; find-acronym: String->Image
; visualize a word with first letter bolded
(check-expect (find-acronym "World")
              (beside
               (text/font (substring "World" 0 1) 24 "black" "Gill Sans" 'modern 'normal 'bold #f)
               (text (substring "World" 1) 24 "black")))
(check-expect (find-acronym "Help")
              (beside
               (text/font (substring "Help" 0 1) 24 "black" "Gill Sans" 'modern 'normal 'bold #f)
               (text (substring "Help" 1) 24 "black")))
(define (find-acronym word)
  (beside
   (text/font (substring word 0 1) 24 "black" "Gill Sans" 'modern 'normal 'bold #f)
   (text (substring word 1) 24 "black")))

