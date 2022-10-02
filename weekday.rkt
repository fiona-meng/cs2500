;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 2, Problem 3 ==

; Your task is to design an interactive weekly exercise
; calendar. The program displays the current day and
; associated exercise, as allows the user to scroll forward/
; backward in the week by pressing the right/left arrow keys,
; respectively. You must do this according to the following
; sequence...


; TODO: Follow the Design Recipe for Weekday, which represents
;       the days of the week (Sunday - Saturday).

; A Weekday is one of:
; - "Sunday"
; - "Monday"
; - "Tuesday"
; - "Wednesday"
; - "Thursday"
; - "Friday"
; - "Saturday"
; interpretation: days in the Weekday
(define W-SU "Sunday")
(define W-MO "Monday")
(define W-TU "Tuesday")
(define W-WE "Wednesday")
(define W-TH "Thursday")
(define W-FR "Friday")
(define W-SA "Saturday")
(define (weekday-temp we)
  (...
   (cond
     [... W-SU ...]
     [... W-MO ...]
     [... W-TU ...]
     [... W-WE ...]
     [... W-TH ...]
     [... W-FR ...]
     [... W-SA ...]
     )))
     


; TODO: Design a function exercise that returns an exercise given
;       a day of the week. Here is an example, but you are
;       free to come up with your own (as long as the activity
;       varies throughout the week, so you don't get bored):
;       - Sunday:    Climbing
;       - Monday:    Cardio
;       - Tuesday:   Upper Body + Core
;       - Wednesday: Cardio
;       - Thursday:  Lower Body + Core
;       - Friday:    Cardio
;       - Saturday:  Rest

; exercise: Weekday -> String
; returns an exercise given a day of the week
(check-expect (exercise W-SU) "Climbing")
(check-expect (exercise W-MO) "Cardio")
(check-expect (exercise W-TU) "Upper Body + Core")
(check-expect (exercise W-WE) "Cardio")
(check-expect (exercise W-TH) "Lower Body + Core")
(check-expect (exercise W-FR) "Cardio")
(check-expect (exercise W-SA) "Rest")
(define (exercise we)
  (cond
    [(string=? we W-SU) "Climbing"]
    [(string=? we W-MO) "Cardio"]
    [(string=? we W-TU) "Upper Body + Core"]
    [(string=? we W-WE) "Cardio"]
    [(string=? we W-TH) "Lower Body + Core"]
    [(string=? we W-FR) "Cardio"]
    [(string=? we W-SA) "Rest"]))


; TODO: Design the functions next-weekday and prev-weekday.
;       The former returns the weekday after that which
;       was supplied (and Monday comes after Sunday); the
;       the latter returns the weekday before that which
;       was supplied (and Sunday comes before Monday).

; next-weekday: Weekday -> Weekday
; returns the weekday after that which was supplied
(check-expect (next-weekday W-SU) W-MO)
(check-expect (next-weekday W-MO) W-TU)
(check-expect (next-weekday W-TU) W-WE)
(check-expect (next-weekday W-WE) W-TH)
(check-expect (next-weekday W-TH) W-FR)
(check-expect (next-weekday W-FR) W-SA)
(check-expect (next-weekday W-SA) W-SU)
(define (next-weekday we)
  (cond
    [(string=? we W-SU) W-MO]
    [(string=? we W-MO) W-TU]
    [(string=? we W-TU) W-WE]
    [(string=? we W-WE) W-TH]
    [(string=? we W-TH) W-FR]
    [(string=? we W-FR) W-SA]
    [(string=? we W-SA) W-SU]))

; prev-weekday: Weekday -> Weekday
; returns the weekday before that which was supplied
(check-expect (prev-weekday W-SU) W-SA)
(check-expect (prev-weekday W-MO) W-SU)
(check-expect (prev-weekday W-TU) W-MO)
(check-expect (prev-weekday W-WE) W-TU)
(check-expect (prev-weekday W-TH) W-WE)
(check-expect (prev-weekday W-FR) W-TH)
(check-expect (prev-weekday W-SA) W-FR)
(define (prev-weekday we)
  (cond
    [(string=? we W-SU) W-SA]
    [(string=? we W-MO) W-SU]
    [(string=? we W-TU) W-MO]
    [(string=? we W-WE) W-TU]
    [(string=? we W-TH) W-WE]
    [(string=? we W-FR) W-TH]
    [(string=? we W-SA) W-FR]))

; TODO: Finally, using these pieces, design the World program
;       exercise-calendar that displays the day and associated
;       exercise, as well as allowing the user to scroll forward/
;       backward in the week by pressing the right/left keys,
;       respectively. (Hint: in BSL, "right" and "left" are the
;       KeyEvent values you need to respond to; but what about
;       all the other keys?) You should supply to this function
;       the initial day of the week to show.

; exercise-calendar : Weekday -> Weekday String 
; displays the day and exercise, as well as allowing the user to scroll
; forward and backward
(define (exercise-calendar initial)
  (big-bang initial
    [to-draw draw-day]
    [on-key change-day]
    ))

; draw-day: Weekday-> Weekday
; display the weekday
(define Background (square 500 "solid" "white"))
(check-expect (draw-day W-MO) (place-image (text "Monday:Cardio" 24 "olive") 150 150 Background))
(check-expect (draw-day W-TU) (place-image (text "Tuesday:Upper Body + Core" 24 "olive") 150 150 Background))
(check-expect (draw-day W-WE) (place-image (text "Wednesday:Cardio" 24 "olive") 150 150 Background))
(check-expect (draw-day W-TH) (place-image (text "Thursday:Lower Body + Core" 24 "olive") 150 150 Background))
(check-expect (draw-day W-FR) (place-image (text "Friday:Cardio" 24 "olive") 150 150 Background))
(check-expect (draw-day W-SA) (place-image (text "Saturday:Rest" 24 "olive") 150 150 Background))
(check-expect (draw-day W-SU) (place-image (text "Sunday:Climbing" 24 "olive") 150 150 Background))
(define (draw-day dd)
  (place-image (text (string-append dd ":" (exercise dd)) 24 "olive") 150 150 Background))

; change-day: KeyEvent-> Weekday
; allowing the user to scroll forward and backward by pressing the right/left keys

(check-expect (change-day  W-SU  "left" ) W-SA)
(check-expect (change-day  W-SA  "left" ) W-FR)
(check-expect (change-day  W-FR  "left" ) W-TH)
(check-expect (change-day  W-TH  "left" ) W-WE)
(check-expect (change-day  W-WE  "left" ) W-TU)
(check-expect (change-day  W-TU  "left" ) W-MO)
(check-expect (change-day  W-MO  "left" ) W-SU)

(check-expect (change-day  W-SU  "right" ) W-MO)
(check-expect (change-day  W-SA  "right" ) W-SU)
(check-expect (change-day  W-FR  "right" ) W-SA)
(check-expect (change-day  W-TH  "right" ) W-FR)
(check-expect (change-day  W-WE  "right" ) W-TH)
(check-expect (change-day  W-TU  "right" ) W-WE)
(check-expect (change-day  W-MO  "right" ) W-TU)

(check-expect (change-day  W-MO  "a" ) W-MO)


(define (change-day we ke)
  (cond
    [(key=? ke "left")(prev-weekday we)]
    [(key=? ke "right") (next-weekday we)]
    [else we]))
   

