;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 1, Problem 2 ==

; TODO #1: design the function full-name that accepts a person's
; first name (e.g., "Grace"), last name (e.g., "Hopper"), and
; desired ordering (either first name then last name, or last name
; then first name), and produces the appropriate full name (e.g.,
; "Grace Hopper" or "Hopper, Grace").
(define-struct name [first last])
; A Name is a (make-name String String)
; Interpretation: people's first name and last name
; - first is people's first name
; - last is people's last name
(define Name-1 (make-name "Grace" "Hopper"))

(define (name-temp n)
  (... (name-first n) ...
       (name-last n) ...))



; A order is one of:
; -"First"
; -"Last"
; Interpretation: first means first name and then last name
; last means last name and then first name
(define F "First")
(define L "Last")

(define (order-temp o)
  (...
   (cond
     [(string=? o "First")]
     [(string=? o "Last")])))
     

; full-name: Name Order->String
; returns people's first name and last name
; by desired order.
(check-expect (full-name Name-1 F ) "Grace Hopper")
(check-expect (full-name Name-1 L) "Hopper, Grace")
(define (full-name n f)
  (cond
    [(string=? f "First") (string-append (name-first n) " " (name-last n))]
    [(string=? f "Last") (string-append (name-last n) ", " (name-first n))]))

                  


