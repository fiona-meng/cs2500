;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6p3-sample) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


(define-struct count [item ct])

; An [ItemCount-of X] is a (make-count X Nat)
; Interpretation: how many times did a value appear?

(define COUNT-STR (make-count "hello" 5))

(define (count-temp c)
  (... (count-item c) ...
       (count-ct c) ...))


; item-counts : (X Y) [List-of X] [X -> Y] [Y Y -> Boolean] -> [List-of [ItemCount-of Y]]
; Counts the distinct elements in a list given a transformation
; function and a transform-equality function

(check-expect
 (item-counts '() string-downcase string=?)
 '())

(check-expect
 (item-counts
  (cons "hello" (cons "world" (cons "HELLO" '())))
  string-downcase
  string=?)
 (cons (make-count "hello" 2)
       (cons (make-count "world" 1) '())))

(define (item-counts lox xf eqf?)
  (cond
    [(empty? lox) '()]
    [(cons? lox)
     (add-to-count
      (xf (first lox))
      eqf?
      (item-counts (rest lox) xf eqf?))]))


; add-to-count : (Y) Y [Y Y -> Boolean] [List-of [ItemCount-of Y]] -> [List-of [ItemCount-of Y]]
; adds an element to the count given an equality function

(check-expect
 (add-to-count
  "hello" string=? '())
 (cons (make-count "hello" 1) '()))

(check-expect
 (add-to-count
  "world" string=?
  (cons (make-count "hello" 1) '()))
 (cons (make-count "hello" 1)
       (cons (make-count "world" 1) '())))

(check-expect
 (add-to-count
  "hello" string=?
  (cons (make-count "hello" 1)
        (cons (make-count "world" 1) '())))
 (cons (make-count "hello" 2)
       (cons (make-count "world" 1) '())))

(check-expect
 (add-to-count
  "world" string=?
  (cons (make-count "hello" 2)
        (cons (make-count "world" 1) '())))
 (cons (make-count "hello" 2)
       (cons (make-count "world" 2) '())))

#|
(define (count-temp c)
  (... (count-item c) ...
       (count-ct c) ...))
|#

(define (add-to-count y eqf? loc)
  (cond
    [(empty? loc) (cons (make-count y 1) '())]
    [(cons? loc)
     (if (eqf? y (count-item (first loc)))
         (cons (increment-count (first loc))
               (rest loc))
         (cons (first loc)
               (add-to-count y eqf? (rest loc))))]))


; increment-count : (Y) [ItemCount-of Y] -> [ItemCount-of Y]
; adds 1 to the count of the supplied count

(check-expect
 (increment-count
  (make-count "hello" 1))
 (make-count "hello" 2))

(define (increment-count c)
  (make-count (count-item c)
              (add1 (count-ct c))))



