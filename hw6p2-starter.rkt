;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 6, Problem 2 ==

; TODO #1: Design the function list-superlative that accepts a
; non-empty list and a value-function (which produces a number
; for each element of the list) and returns the first element
; that results in the maximal valuation. This is essentially an
; argmax and so you aren't allowed to use this function; you
; must produce a template-based, recursive solution for credit.
;
; As guiding examples...
; - given a list of the first three positive integers (1, 2, 3)
;   and the value-function identity (which just returns whatever
;   it is supplied), the function would return 3; however, if
;   the function were - (which negates the values), the function
;   would return 1.
; - given a list of (x, y) positions and posn-y as the
;   value-function, the function should return the position with
;   the largest y-coordinate
; - given a list of strings and string-length as the value-function,
;   the function should return the longest string

; A [NEList-of Number] is one of:
; -(cons Number '())
; -(cons Number [NEList-of X])
; Interpretation: a non-empty list of Number
(define lon-1 (cons -3 (cons -2 (cons -1 '()))))
(define lon-2 (cons -88 (cons -8 (cons -9999 '()))))
(define (nelon-temp lon)
  (...
   (cond
     [(empty? (rest lon)) ... (first lon) ... ]
     [(cons? (rest lon)) ...
      (first lon)
      (nelon-temp(rest lon))...])))

; negate: [list-of Number]->Number
; find the maximum of a list of negate number
(check-expect (negate lon-1) 3)
(check-expect (negate lon-2) 88)
(define (negate lon)
  (cond
    [(empty? (rest lon)) (first lon)]
    [(cons? (rest lon))
     (max
      (negate-number (first lon))
      (negate (rest lon)))]))

; negate-number : Number -> Number
; negate the number
(check-expect (negate-number 5) -5)
(check-expect (negate-number -10) 10)
(define (negate-number number)
  (* number -1))


; list-superlative: list-superlative
; find the original number of the maximal number
(check-expect (list-superlative lon-1) -3)
(check-expect (list-superlative lon-2) -88)
(define (list-superlative lon)
  (negate-number (negate lon)))


