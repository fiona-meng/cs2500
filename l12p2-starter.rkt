;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname l12p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Lab 12, Problem 2 ==

; TODO #1: design the function my-build-list that works exactly like build-list.
; You may not use build-list to implement it. Instead, use an accumulator to
; count up from 0 to the natural number supplied to the function (and of course
; don't forget your Accumulator statement!).

; my-build-list: (X) Nat [Nat-> X] -> [List-of X]
; implement the build-list function

(check-expect (my-build-list 10 add1) (build-list 10 add1))
(check-expect (my-build-list 10000 add1) (build-list 10000 add1))

(define (my-build-list n f)
  (local [; my-build-list/acc: Nat [List-of Nat] Nat-> [List-of Nat]
          ; implements build list, given the accumulator
          ; Accumulator: the build-list value so far
          (define (my-build-list/acc n acc b)
            (if (= (length acc) n)
                (reverse acc)
                (my-build-list/acc n (cons (f b) acc) (add1 b))))]
    (my-build-list/acc n '() 0)))




