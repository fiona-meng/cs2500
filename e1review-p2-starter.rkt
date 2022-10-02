;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1review-p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
  (... (chair-color c) 
       (chair-back? c) ...))


; A CouchSize (CS) is one of:
; - "loveseat"
; - "3-seat"
; - "L-shape"
; Interpretation : different sizes of a couch

(define CS-1 "loveseat")
(define CS-2 "3-seat")
(define CS-3 "L-seat")
(define (couchsize-temp c)
  (...
   (cond
     [(string=? c CS-1) ...]
     [(string=? c CS-2) ...]
     [(string=? c CS-3) ...])))
 

(define-struct couch [color type])

; A Couch is a (make-couch String CouchSize)
; Interpretation : a comfortable place for you to sit
; - color is the color of the couch
; - type is the category of couch

(define COUCH-1 (make-couch "orange" CS-1))
(define COUCH-2 (make-couch "medium cyan" CS-2))
(define COUCH-3 (make-couch "yellow" CS-3))
(define (couch-temp c)
  (...
   (cond
     [(couch-color) ...]
     [(couchsize-temp(couch-type)) ...])))


; (a) Complete the design of all the data definitions by providing
; correct templates.



; (b) Design the data Seat, which can be either a chair or couch.
; A Seat is one of:
; -"chair"
; -"couch"
; A seat can be a chair or a couch
(define Seat-1 CHAIR-1)
(define Seat-2 CHAIR-2)
(define Seat-3 COUCH-1)
(define Seat-4 COUCH-2)
(define Seat-5 COUCH-3)
(define (seat-temp c)
  (...
   (cond
     [(chair? c) (chair-temp c) ...]
     [(couch? c) (couch-temp c) ...])))
     

; (c) Design the function seat-catalog that accepts a Seat and returns a String that is
; a catalog order for the specified type of Seat. An example catalog order for a Chair
; might be "Chair : pink". An example catalog order for a Couch might be
; "Couch : yellow, loveseat".

; seat-catalog: Seat->String
; returns the catalog of the seat
(check-expect (seat-catalog Seat-1) "Chair : purple")
(check-expect (seat-catalog Seat-2) "Chair : green")
(check-expect (seat-catalog Seat-3) "Couch : orange, loveseat")
(check-expect (seat-catalog Seat-4) "Couch : medium cyan, 3-seat")
(check-expect (seat-catalog Seat-5) "Couch : yellow, L-seat")
(define (seat-catalog c)
  (cond
    [(chair? c) (string-append "Chair : " (chair-color c))]
    [(couch?  c) (string-append "Couch : "(couch-color c) ", " (couch-type c))]))


; (d) Design the function loveseat? that accepts a Seat and determines if
; it is a loveseat.

; loveseat?: Seat-> Boolean
; determine whether it is a loveseat
(check-expect (loveseat? Seat-1) #false)
(check-expect (loveseat? Seat-2) #false)
(check-expect (loveseat? Seat-3) #true)
(check-expect (loveseat? Seat-4) #false)
(check-expect (loveseat? Seat-5) #false)
(define (loveseat? c)
  (cond
     [(chair? c) #false]
     [(couch? c)
      (string=? (couch-type c) "loveseat")]))
  
