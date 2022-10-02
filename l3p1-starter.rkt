;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname l3p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Lab 3, Problem 1 ==

; Consider the following data definitions & interpretations:

(define-struct address [num st city us-state zip])

; An Address is a (make-address Nat String String String Nat)
; - where num is the number of the building on the street
; - st is the name of the street
; - city is the city the building is in
; - us-state is the state the city is in
; - and zip is the zipcode of the building
; Interpretation: a US address
(define Address-1 (make-address 1 "Newton" "Boston" "MA" 12345))
(define (address-temp add)
  (... (address-num add)...
   ... (address-st add) ...
   ... (address-city add) ...
   ... (address-us-state add) ...
   ... (address-zip add) ...))
    
(define-struct student [first last nuid local perm])

; An NUStudent is a (make-student String String PositiveNumber Address Address)
; - where first is the student's first name
; - last is the student's last name
; - nuid is the student's NUID #
; - local is the student's local address
; - and perm is the student's permanent address
; Interpretation: a Northeastern student
(define NUStudent-1 (make-student "fanyue" "meng" 123456 "1 Newton Boston MA 11111" "2 chaoyang Beijing Beijing 12347"))
(define (nustudent-temp nus)
  (... (student-first nus) ...
   ... (student-last nus) ...
   ... (student-nuid nus) ...
   ... (student-local nus) ...
   ... (student-perm nus) ...))

; TODO #1: complete the data design recipe for the
; above data definitions.

; TODO #2: Design the function student-email
; which takes an NUStudent and produces a string
; representing that student’s email address.
; For simplicity we will say that a student’s email
; address is always their last name (all lowercase),
; followed by a period, followed by the first initial
; of their first name (also lowercase), and finished
; with "@northeastern.edu".

; student-email: NUStudent-> String
; produce a email addree for a Northeastern student
(check-expect (student-email NUStudent-1) "meng.f@northeastern.edu")
(define (student-email nus)
  (string-append (string-downcase(student-last nus)) "." (string-downcase(substring (student-first nus) 0 1)) "@northeastern.edu"))

; TODO #3: Design the function update-zipcode which
; takes an NUStudent and a number, representing the
; new zip code of the person and updates their permanent
; address to have that zip code. Be sure to follow the
; template!
; update-zipcode: NUStudent-> String
; return a student but with its zipcode changed
(check-expect (update-zipcode NUStudent-1 12345)
              (make-student "Hermione" "Granger" 965 
(define (student-email nus Zip)
  (make-student (student-first nus) (student-last nus) (student-nuid nus) (student-local nus) (make-address (address-num (student-perm nus))
                                                                                                            (address-st (student-perm nus))
                                                                                                            (address-city (student-perm nus))
                                                                                                            (address-us-state (student-perm nus))
                                                                                                            (address-zip Zip))))
                                                                                                            