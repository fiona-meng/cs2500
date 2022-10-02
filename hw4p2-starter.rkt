;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 4, Problem 2 ==

; Consider the following data definition:

(define-struct processing [estimate])
(define-struct shipped [estimate])

; A PackageStatus(PS) is one of:
; - (make-processing String)
; - (make-shipped String)
; - "on the truck"
; - #true
; Interpretation: status of a package delivery,
; either processing (i.e., not yet shipped) with
; an expected ship date, already shipped with
; an expected delivery date, on the truck for
; delivery today, or already delivered (#true)
(define PS-1 (make-processing "3/14/2021"))
(define PS-2 (make-shipped "3/14/2021"))
(define PS-3 "on the truck")
(define PS-4  "#true")
(define (ps-temp p)
  (...
   (cond
     [(processing? p) ...
      ... (processing-estimate p) ...]
     [(shipped? p)
      ... (shipped-estimate p) ...]
     [(string=? p) ...]
     [(euqal? p) ...])))

; TODO #1: finish the data design recipe for PackageStatus

; TODO #2: design the function package-update,
; which given a package label (e.g., "my new ipad"),
; number of items in the shipment (e.g., 1), and
; PackageStatus, and outputs a status update.

; Some example status updates include:
; - "my new ipad (1 item) is still processing and should ship on 3/14/2021"
; - "newest HP spinoff (2 items) has shipped and should arrive on 3/14/2021"
; - "tasty cookies (4 items) is on the truck for delivery today"
; - "red stapler (1 item) has been delivered"

; Hint: if you find that the right-hand side of your cond
; branches repeat a lot of the same work, you should move
; some of that work out of the cond (possibly into helper
; functions) so your code doesn’t repeat itself.

; package-update: String Number PS -> String
; returns the information of the package with package name, number of items, and PackageStatus
(check-expect (package-update "my new ipad" 1 PS-1)
              "my new ipad (1 item) is still processing and should ship on 3/14/2021")

(check-expect (package-update "newest HP spinoff" 2 PS-2)
              "newest HP spinoff (2 items) has shipped and should arrive on 3/14/2021")

(check-expect (package-update "tasty cookies" 4 PS-3)
              "tasty cookies (4 items) is on the truck for delivery today")

(check-expect (package-update "red stapler" 1 PS-4)
              "red stapler (1 item) has been delivered")
(define (package-update a b c)
  (string-append a " (" (number->string b) (check-number b) (check-status c)))
; check-status: PackageStatus-> String
; returns the status
(check-expect (check-status PS-1)
              "is still processing and should ship on 3/14/2021")
(check-expect (check-status PS-2)
              "has shipped and should arrive on 3/14/2021")
(check-expect (check-status PS-3)
              "is on the truck for delivery today")
(check-expect (check-status PS-4)
              "has been delivered")

(define (check-status c)
   (cond
     [(processing? c) (string-append "is still processing and should ship on "(processing-estimate c))]
     [(shipped? c) (string-append "has shipped and should arrive on "(shipped-estimate c))]
     [(string=? c "on the truck") "is on the truck for delivery today"]
     [(string=? c "#true") "has been delivered"]))
; check-number: Number-> String
; determine it is item or itmes
(check-expect (check-number 1) " item) ")
(check-expect (check-number 99) " items) ")
(define (check-number b)
  (if (> b 1) " items) " " item) ")) 