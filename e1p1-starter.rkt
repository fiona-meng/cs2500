;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 1, Problem 1 ==

; Consider the following data definitions and examples:


; A PetEnergy (PE) is one of:
; - "low"
; - "medium"
; - "high"
; Interpretation: energy level of a pet

(define PE-LOW "low")
(define PE-MEDIUM "medium")
(define PE-HIGH "high")
(define (pe-temp pe)
  (...
   (cond
     [(string=? pe PE-LOW) ...]
     [(string=? pe PE-MEDIUM) ...]
     [(string=? pe PE-HIGH) ...])))
     
  

(define-struct pet [name energy age])

; A Pet is a (make-pet String PetEnergy PosInt)
; Interpretation: a human's friend :)
; - name is what humans call the pet
; - energy is the pet's energy level
; - age is the pet’s age in months

(define PET-1 (make-pet "odie" PE-HIGH 24))
(define PET-2 (make-pet "garfield" PE-LOW 60))
(define (pet-temp pet)
  (... (pet-name pet) ...
       (pe-temp (pet-energy pet)) ...
       (pet-age pet) ...))


; TODO #1: produce templates for PetEnergy and Pet

