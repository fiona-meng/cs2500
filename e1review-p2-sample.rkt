;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1review-p2-sample) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 1 Review, Problem 2 ==

; Consider the following data definitions:


(define-struct chair [color back?])

; A Chair is a (make-chair String Boolean)
; Interpretation : a 4-legged place for you to sit
; - color is the color of the chair
; - back? is true if it has a back

(define CHAIR-1 (make-chair "purple" #true))
(define CHAIR-2 (make-chair "green" #false))

(define (chair-temp c)
  (... (chair-color c) ...
       (chair-back? c) ...))


; A CouchSize (CS) is one of:
; - "loveseat"
; - "3-seat"
; - "L-shape"
; Interpretation : different sizes of a couch

(define CS-1 "loveseat")
(define CS-2 "3-seat")
(define CS-3 "L-seat")

(define (cs-temp cs)
  (...
   (cond
     [(string=? cs CS-1) ...]
     [(string=? cs CS-2) ...]
     [(string=? cs CS-3) ...])))


(define-struct couch [color type])

; A Couch is a (make-couch String CouchSize)
; Interpretation : a comfortable place for you to sit
; - color is the color of the couch
; - type is the category of couch

(define COUCH-1 (make-couch "orange" CS-1))
(define COUCH-2 (make-couch "medium cyan" CS-2))
(define COUCH-3 (make-couch "yellow" CS-3))

(define (couch-temp c)
  (... (couch-color c) ...
       (cs-temp (couch-type c)) ...))


; (a) Complete the design of all the data definitions by providing
; correct templates.



; (b) Design the data Seat, which can be either a chair or couch.

; A Seat is one of:
; - Chair
; - Couch
; Interpretation : a place to sit

(define S-1 CHAIR-1)
(define S-2 CHAIR-2)

(define S-A COUCH-1)
(define S-B COUCH-2)
(define S-C COUCH-3)

(define (seat-temp s)
  (...
   (cond
     [(chair? s) ... (chair-temp s) ...]
     [(couch? s) ... (couch-temp s) ...])))



; (c) Design the function seat-catalog that accepts a Seat and returns a String that is
; a catalog order for the specified type of Seat. An example catalog order for a Chair
; might be "Chair : pink". An example catalog order for a Couch might be
; "Couch : yellow, loveseat".

; seat-catalog : Seat -> String
; returns the catolog order for the specified type of Seat

(check-expect (seat-catalog S-1) "Chair : purple")
(check-expect (seat-catalog S-2) "Chair : green")
(check-expect (seat-catalog S-A) "Couch : orange, loveseat")
(check-expect (seat-catalog S-B) "Couch : medium cyan, 3-seat")
(check-expect (seat-catalog S-C) "Couch : yellow, L-seat")

(define (seat-catalog s)
  (cond
    [(chair? s) (string-append "Chair : " (chair-color s))]
    [(couch? s) (string-append "Couch : " (couch-color s) ", " (couch-type s))]))



; (d) Design the function loveseat? that accepts a Seat and determines if
; it is a loveseat.

; loveseat? : Seat -> Boolean
; is the supplied seat a loveseat?

(check-expect (loveseat? S-1) #false)
(check-expect (loveseat? S-2) #false)
(check-expect (loveseat? S-A) #true)
(check-expect (loveseat? S-B) #false)
(check-expect (loveseat? S-C) #false)

(define (loveseat? s)
  (cond
    [(chair? s) #false]
    [(couch? s) (loveseat?/couch s)]))


; loveseat?/couch : Couch -> Boolean
; is the supplied couch a loveseat?

(check-expect (loveseat?/couch COUCH-1) #true)
(check-expect (loveseat?/couch COUCH-2) #false)
(check-expect (loveseat?/couch COUCH-3) #false)

(define (loveseat?/couch c)
  (loveseat?/cs (couch-type c)))


; loveseat?/cs : CS -> Boolean
; is the supplied type of couch a loveseat?

(check-expect (loveseat?/cs CS-1) #true)
(check-expect (loveseat?/cs CS-2) #false)
(check-expect (loveseat?/cs CS-3) #false)

(define (loveseat?/cs cs)
  (cond
    [(string=? cs CS-1) #true]
    [(string=? cs CS-2) #false]
    [(string=? cs CS-3) #false]))


