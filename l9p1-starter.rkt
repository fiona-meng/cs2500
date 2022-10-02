;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname l9p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; == Lab 9, Problem 1 ==

; You are to design a set of functions below. Each has a set of
; restrictions that apply to the problem, so pay attention to
; the instructions for each TODO.


; TODO #1: design the function alternate which, given two lists
; produces a list of alternating elements from each list (and if
; one list runs out of elements you should place all the remaining
; elements in the other list at the end). Some tests have been
; supplied for clarity.

; Restrictions:
; - Do NOT use ISL list abstractions.


; alternate : [List-of Any] [List-of Any] -> [List-of Any]
; produces a list resulting from alternating between the two
; supplied lists


(check-expect
 (alternate '() '())
 '())

(check-expect
 (alternate '() (list 1 "a"))
 (list 1 "a"))

(check-expect
 (alternate (list 1 2 3) '())
 (list 1 2 3))

(check-expect
 (alternate (list 1 2)
            (list "a" "b" "c"))
 (list 1 "a" 2 "b" "c"))

(check-expect
 (alternate (list "a" "b" "c") '())
 (list "a" "b" "c"))


(define (alternate x1 x2)
  (cond
    [(and (empty? x1) (empty? x2)) '()]
    [(and (empty? x1) (cons? x2))
     (cons (first x2) (rest x2))]
    [(and (cons? x1) (empty? x2))
     (cons (first x1) (rest x1))]
    [(and (cons? x1) (cons? x2))
     (cons (first x1)
           (cons (first x2)
                 (alternate(rest x1) (rest x2))))]))


; TODO #2: Design the function cross-product which, given two lists
; produces a cross product of their elements. Some tests have been
; supplied for clarity.

; Restrictions:
; - Do NOT use ISL list abstractions.


; cross-product : [List-of Any] [List-of Any] -> [List-of (list Any Any)]
; produces the cross product of the two lists


(check-expect
 (cross-product '() '())
 '())

(check-expect
 (cross-product (list 1 2) '())
 '())

(check-expect
 (cross-product '() (list 1 2))
 '())

(check-expect
 (cross-product (list 1 2) (list "a" "b" "c"))
 (list (list 1 "a")
       (list 1 "b")
       (list 1 "c")
       (list 2 "a")
       (list 2 "b")
       (list 2 "c")))

(define (cross-product l1 l2)
  (local[; cross: Any [List-of Any] -> [List-of Any]
         ; distribute element into a list of element
         (define (cross x lox-2)
           (cond
             [(empty? lox-2) '()]
             [(cons? lox-2)
              (cons
               (list x (first lox-2))
               (cross x (rest lox-2)))]))
         ; cross-adv:[List-of Any] -> [List-of Any]
         ; returns elements that show up in l2
         (define (cross-adv lox-1)
           (cond
             [(empty? lox-1) '()]
             [(cons? lox-1)
              (append (cross (first lox-1) l2)
                    (cross-adv (rest lox-1)))]))]
    (cross-adv l1)))

; TODO #3: design the function mymap2, which takes in two lists and returns
; the list of results of performing on operation on the first from each list,
; the second, and so on. If one of the list runs out of elements, then stop
; producing results. Some tests have been supplied for clarity.

; Restrictions:
; - Do NOT use ISL list abstractions.


; mymap2 : [List-of Any] [List-of Any] [Any Any -> Any] -> [List-of Any]
; produces a result list by applying the function to pairs of elements
; from the supplied list (until one is empty)


(check-expect
 (mymap2 '() '() *)
 '())

(check-expect
 (mymap2 (list 2 3) (list 4) *)
 (list 8))

(check-expect
 (mymap2 (list "a" "b")
         (list "1" "2" "3")
         string-append)
 (list "a1" "b2"))

(check-expect
 (mymap2 (list "a" "c")
         (list (list "x") (list 3 1 4))
         (λ (s l) (string-append
                   s
                   "-"
                   (number->string (length l)))))
 (list "a-1" "c-3"))


(define (mymap2 l1 l2 eq)
  (cond
    [(or (empty? l1) (empty? l2)) '()]
    [(and (cons? l1) (cons? l2))
     (cons
     (eq (first l1) (first l2))
     (mymap2(rest l1) (rest l2) eq))]))

; TODO #4: design the function read-lines-or-empty that attempts to read
; the lines of a file (via a supplied name) each as a string in a list,
; or returns an empty list if the file is not found. Some tests have been
; supplied for clarity.

; Restrictions:
; - Do NOT use ISL list abstractions.


; read-lines-or-empty : String -> [List-of String]
; attempts to read the lines of a file


(define BADFILE "BADFILENAME.SAD")

(check-expect
 (read-lines-or-empty BADFILE)
 '())

(check-expect
 (read-lines-or-empty "progs.txt")
 (list "CS" "DS" "CY"))

(check-expect
 (read-lines-or-empty "nums.txt")
 (list "1800" "3000" "2550" "4300"))


(define (read-lines-or-empty file-name)
  (if (file-exists? file-name)
  (read-lines file-name)
  '()))

; TODO #5: design the function cat-files, which takes two files and produces
; a list of all the lines of the first file (as strings) followed by the
; second. If either file doesn't exist, interpret that as having no lines.
; Some tests have been supplied for clarity.

; Restrictions:
; - Do NOT use ISL list abstractions.
; - You are NOT allowed to use append; that would make it a bit too easy ;)

; (But yes, you are encouraged to make use of prior functions you wrote
; in this file to solve this problem.)


; cat-files : String String -> [List-of String]
; concatenates the supplied files


(check-expect
 (cat-files "progs.txt" "nums.txt")
 (list "CS" "DS" "CY"
       "1800" "3000" "2550" "4300"))

(check-expect
 (cat-files "progs.txt" BADFILE)
 (list "CS" "DS" "CY"))

(check-expect
 (cat-files BADFILE "progs.txt")
 (list "CS" "DS" "CY"))


(define (cat-files file-1 file-2)
  (cond
    [(empty? (read-lines file-1)) file-2]
    [(cons? (read-lines file-1))
     (cons
     (first (read-lines file-1))
     (cat-files (rest (read-lines file-1))file-2))]))




; TODO #6: design the function cat-lines, which takes two files and produces
; the result of concatenating all their lines together (with an intermediate
; space). Some tests have been supplied for clarity.

; Restrictions:
; - Do NOT use ISL list abstractions.

; (But yes, you are encouraged to make use of prior functions you wrote
; in this file to solve this problem.)


; cat-lines : String String -> [List-of String]
; glues together the matching file lines


(check-expect
 (cat-lines "progs.txt" "nums.txt")
 (list "CS 1800" "DS 3000" "CY 2550"))

(check-expect
 (cat-lines "progs.txt" BADFILE)
 '())

(check-expect
 (cat-lines BADFILE "progs.txt")
 '())

(define (cat-lines l1 l2)
  (local [; String String -> String
          ; returns a string by finding
          ; the content in both of files
          (define (match x1 x2)
            (string-append x1 " " x2))]
  (cond
    [(not (and (cons? (read-lines l1)) (cons? (read-lines l1)))) '()]
    [(and (cons? (read-lines l1)) (cons? (read-lines l1)))
     (cons
     (match (first (read-lines l1)) (first (read-lines l2)))
     (cat-lines (rest (read-lines l1)) (rest (read-lines l2))))])))
