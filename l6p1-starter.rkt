;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname l6p1-starter) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; == Lab 6, Problem 1 ==

; Consider the following:


; A Posn is a (make-posn Real Real)
; Interpretation: an (x, y) coordinate

; matching-x-posn : [List-of Posn] Number Posn -> Posn
; Find the first Posn in the list with the given x-coordinate
; or return the given Posn if no such position can be found

(check-expect
 (matching-x-posn
  '()
  10 (make-posn 0 0))
 (make-posn 0 0))

(check-expect
 (matching-x-posn
  (cons (make-posn 1 2) (cons (make-posn 3 4) '()))
  3 (make-posn 5 6))
 (make-posn 3 4))


(define (matching-x-posn lop desired-x default)
  (find-first-match lop desired-x default match?))

; match?: Posn Number->Boolean
; determine whether the position match
(check-expect (match? (make-posn 1 2) 1) #true)
(check-expect (match? (make-posn 1 2) 5) #false)

(define (match? p desired-x)
  (= (posn-x p) desired-x))
              


; string-with-length : [List-of String] Nat -> String
; Returns the first String in the given list with the given
; length or "no such string" if no such string can be found

(check-expect
 (string-with-length
  '()
  10)
 "no such string")

(check-expect
 (string-with-length
  (cons "hi" (cons "hello" (cons "aloha" '())))
  5)
 "hello")

(define (string-with-length los desired-length)
  (find-first-match los desired-length "no such string" length?))

; length?: String Number->Boolean
; determine wether the string length match
(check-expect (length? "hello" 5)
              #true)
(check-expect (length? "hello" 7)
              #false)

(define (length? los desired-length)
  (= (string-length los) desired-length))


; TODO #1: design the function find-first-match that abstracts
; these two functions. Be sure to redefine matching-x-posn and
; string-with-length using your abstraction.

; find-first-match: (X Y) [list-of X] X [X Y->Boolean]->X
; find the appropriate math for each function
(check-expect (find-first-match
               (cons "a"
                     (cons "b"
                           (cons "c" '())))
               "lalala"
               "DNE"
               string=?
               )
              "DNE")
(check-expect (find-first-match
               (cons "a"
                     (cons "b"
                           (cons "c" '())))
               "c"
               "DNE"
               string=?
               )
              "c")

(define (find-first-match lox desired-x default cf)
  (cond [(empty? lox) default]
        [(cons? lox)
         (if (cf (first lox) desired-x)
             (first lox)
             (find-first-match (rest lox) desired-x default cf))]))



; TODO #2: using find-first-match, design the function any-true?
; that returns #true if a list of Boolean data contains at least
; one #true, otherwise #false

; any-true?: (X Y) [list-of X] X [X Y->Boolean]->Boolean
; determine any of those contains one #true
(check-expect (any-true?
               (cons "a"
                     (cons "b"
                           (cons "c" '())))
               "c"
               string=?
               )
              #true)
(check-expect (any-true?
               (cons "a"
                     (cons "b"
                           (cons "c" '())))
               "yellow"
               string=?
               )
              #false)

(define (any-true? lox desired-x cf)
  (cond [(empty? lox) #false]
        [(cons? lox)
         (if (cf (first lox) desired-x)
             #true
             (any-true? (rest lox) desired-x cf))]))





