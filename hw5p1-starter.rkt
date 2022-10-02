;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw5p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 5, Problem 1 ==

; TODO #1: Design a data definition for a list of strings.

; A ListOfStrings (LoS) is one of:
; - '()
; - (cons String LoS)
; Interpretation: a list of strings
(define LOS-1
  (cons "My name is Tom"
        (cons "World"
              (cons "Hello" '()))))
(define LOS-2
  (cons "My name is Tom"
        (cons "World"
              (cons "Hello"
                    (cons "Hello" '())))))


(define (los-temp los)
  (...
   (cond
     [(empty? los) ...]
     [(cons? los) ...
      (first los)...
      (los-temp (rest los))...])))

; TODO #2: Design the function any-longer? that determines
; if any string in a supplied list of strings is longer
; than a supplied string.

; any-longer?: LoS String-> Boolean
; determine whether any the list of string is longer
; than a supplied string
(check-expect (any-longer? LOS-1 "Hi") #true)
(check-expect (any-longer? LOS-1 "This is a very long word") #false)
(define (any-longer? los supplied-string)
  (cond
    [(empty? los) #false]
    [(cons? los)
     (or
      (>(string-length (first los)) (string-length supplied-string))
      (any-longer? (rest los) supplied-string))]))


; TODO #3: Design the function num-occurrences that counts
; the number of times a supplied string occurs within a
; list of strings.

; num-occurrences: LoS String->Number
; find the times that a supplied string occurs
(check-expect (num-occurrences LOS-2 "Hello") 2)
(define (num-occurrences los supplied-string)
  (cond
    [(empty? los) 0]
    [(cons? los) 
     (+
      (if (string=? (first los) supplied-string) 1 0)
      (num-occurrences (rest los)supplied-string))]))

; TODO #4: Design the function remove-occurrences that returns
; a list of strings with all occurrences of a supplied string
; removed from a supplied list of strings.

; remove-occurrences: LoS String-> LoS
; remove all the same string
(check-expect (remove-occurrences LOS-1 "Hello")
              (cons "My name is Tom"
                    (cons "World" '())))
(define (remove-occurrences los supplied-string)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (string=? (first los) supplied-string)
         (remove-occurrences (rest los) supplied-string)
         (cons (first los)
               (remove-occurrences (rest los) supplied-string)))]))