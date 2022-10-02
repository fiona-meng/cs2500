;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3p1-meng) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 3, Problem 1 ==

; Consider the following structure and data definition:

(define-struct element [name symbol num weight])

; An Element is a (make-element String String PosInteger PosReal)
; Interpretation: an element on the periodic table
; - name is the name of the element
; - symbol is the element's symbol
; - num is the atomic number
; - weight is the standard atomic weight

; TODO #1: List all signatures of all functions that are defined by
; this structure and data definition. Your signatures should
; be as precise as possible.

;contructor
;make-element : String String PosInteger PosReal -> Element
;predicate
;element? : String String PosInteger PosReal -> String
;seletors
; element-name : element -> String
; element-symbol : element -> String
; element-num : element -> PosInteger
; element-weight : element -> PosReal


; TODO #2: Define at least three examples of Element
; (feel free to reference relevant sources, such as ...
; https://en.wikipedia.org/wiki/Periodic_table)

(define Element-1 (make-element "Hydrogen" "H" 1 1.008))
(define Element-2 (make-element "Iron" "Fe" 26 55.845))
(define Element-3 (make-element "Carbon" "C" 6 12.011))


; TODO #3: Design the template for functions that consume an Element

(define (element-temp el)
  (... (element-name el) ...
       (element-symbol el) ...
       (element-num el) ...
       (element-weight el) ...))
