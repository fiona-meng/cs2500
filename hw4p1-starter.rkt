;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 4, Problem 1 ==

; TODO: design the data necessary to represent a book, which can
; either be physical or electronic. All books have a title, author
; and publication year. Physical books are either paperback or
; harcover, and have some number of pages. Electronic (e-books)
; have a format (pdf, epub, txt, azw, html) and a source URL.

; A book-type (bt) is one of:
; -"paperback"
; -"hardcover"
; Interpretation: types of the book
(define bt-p  "paperback")
(define bt-h "hardcover")
(define (bt-temp bt)
  (...
   (cond
     [(string=? bt bt-p) ...]
     [(string=? bt bt-h) ...])))

(define-struct physical [type pages])
; Physical book is a (make-physical book-type Nat)
; - where type is the type of the book
; it either can be paperback or hardcover
; - pages is how many pages in the book
; Interpretation: physical book
(define physical-1 (make-physical bt-p 100))
(define physical-2 (make-physical bt-h 300))
(define (physical-temp p)
  (... (bt-temp(physcial-type p)) ...
       (physcial-pages p) ...))
; A Ebook-format (ef) is one of:
; -"pdf"
; -"epub"
; -"txt"
; -"azw"
; -"html"
; Interpretation: types of ebook
(define ef-p "pdf")
(define ef-e "epub")
(define ef-t "txt")
(define ef-a "azw")
(define ef-h "html")
(define (ef-temp ef)
  (...
   (cond
     [(string=? ef ef-p) ...]
     [(string=? ef ef-e) ...]
     [(string=? ef ef-t) ...]
     [(string=? ef ef-a) ...]
     [(string=? ef ef-h) ...])))
     
(define-struct electronic [format URL])
; a electronic book is a (make-electronic Ebook-format String)
; -where format is the format of the electronic book
; -URL is the address of the website
; Interpretation: electronic book
(define electronic-1 (make-electronic ef-p "www.example.com"))
(define electronic-2 (make-electronic ef-e "www.husky.com"))
(define electronic-3 (make-electronic ef-t "www.north.com"))
(define electronic-4 (make-electronic ef-a "www.east.com"))
(define electronic-5 (make-electronic ef-h "www.northeastern.com"))
(define (electronic-temp e)
  (... (ef-temp (electronic-format e)) ...
       (electronic-URL e) ...))

(define-struct book [title author publication-year])
; a book is a (make-book String String Nat)
; - title is the title of the book
; - author is the writer
; - publication-year is the year when the book were published
; Interpretation: the information of the book
(define book-1 (make-book "To Kill a Mockingbird" "Harper Lee" 1960))
(define (book-temp b)
  (... (book-title b) ...
       (book-author b) ...
       (book-publication-year b) ...)) 

