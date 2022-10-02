;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 6, Problem 3 ==

; TODO #1: design the function item-counts that accepts a list of
; elements, a transformation function, and an equality function,
; and counts the distinct transformation results.

; The transformation function takes an element from the list and
; produces a result. The equality function takes two results of
; the transformation function and determines if they are the same.

; As a motivating example, consider counting words (supplied as
; strings): the transformation might convert each string to lower-case,
; the equality would then compare two strings to see if they are
; equal; and the resulting counts would be a list of pairings of
; distinct-lower-case words and how many times they appeared in the
; original list. SO... in case of Peter Piper
; (https://en.wikipedia.org/wiki/Peter_Piper), supplied as a list of
; strings without punctuation, you would learn that "peter" occurs 4
; times (as do "piper", "picked", "pickled", and "peppers"); "a"
; appears 3 times, and "if" appears 1 time (as does "where", "is",
; and "the").

; The function result should be a list, where each element is a
; pairing between a distinct transformation result and a count
; of how many times that result has occurred in the original list.
; You should design this data as a first step.

; You'll then design item-counts to consider each element of the
; supplied list in order. For each element, transform it and then
; add the result to the count. Adding is a bit tricky: first you
; check if that result has already been seen (and, if so, +1 to the
; associated counter); otherwise, add a new count pair of 1 to the end.


; A [List-of Number](Lon) is one of:
; -'()
; -(cons Number Lon)
; Interpretation: a list of numbers
(define Lon-0 '())
(define Lon-1 (cons 99
                    (cons -99
                          (cons 80
                                (cons 20
                                      (cons 99
                                            (cons -20 (cons 20 '()))))))))
(define (lon-temp lon)
  (...
   (cond
     [(empty? lon) ...]
     [(cons? lon) ...
      (first lon)
      (lon-temp(rest lon)) ...])))
; transformation: [List-of Number]-> [List-of Number]
; absolute a list of number
(check-expect (transformation Lon-1)
              (cons 99
                    (cons 99
                          (cons 80
                                (cons 20
                                      (cons 99
                                            (cons 20 (cons 20 '()))))))))
(check-expect (transformation (cons -9999 (cons -20 '())))
              (cons 9999 (cons 20 '())))
(define (transformation lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (cons
      (abs (first lon))
      (transformation(rest lon)))]))
; remove-duplicates: [List-of Number]-> [List-of Number]
; remove the repeated number and return 
(check-expect (remove-duplicates Lon-1)
              (cons -99 (cons 80 (cons 99 (cons -20 (cons 20 '()))))))

(define (remove-duplicates lon)
  (cond [(empty? lon) '()]
        [(member (first lon) (rest lon))
         (remove-duplicates (rest lon))]
        [else
         (cons (first lon)
               (remove-duplicates (rest lon)))]))

(define-struct pair[number time])
; A pair is a (make-pair Number Number)
; -numner is the distinct number in the list
; -time is times when a distinct number appear in the list
(define pair-1 (make-pair 99 1))
(define pair-2 (make-pair 99 2))
(define (pair-tmep lon)
  (... (pair-number lon) ...
       (pair-time lon) ...))


