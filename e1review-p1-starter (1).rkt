;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |e1review-p1-starter (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 1 Review, Problem 1 ==

; Consider the following data definitions:

(define-struct slide [title animations? content time])

; A Slide is a (make-slide String Boolean String NaturalNumber)
; Interpretation: a single slide in a presentation
; - title represents the title of the slide
; - animations? represents whether or not the slide has animations
; - content represents the content of the slide
; - time represents the seconds it takes for the slide to present

(define SLIDE-1
  (make-slide "Why Fundies is Great"
              #true
              "A presentation made by Erica + Cat + Steven" 20))

(define SLIDE-2
  (make-slide "Reason #1"
              #false
              "Design Recipe!" 30))

(define SLIDE-3
  (make-slide "Reason #2"
              #false
              "Nate Derbinsky's Tik Tok's" 40))

(define SLIDE-4
  (make-slide "Reason #3"
              #true
              "The TA's are fabulous" 20))

(define (slide-temp s)
  (... (slide-title s) ...
       (slide-animations? s)
       (slide-content s)
       (slide-time)))


(define-struct presentation [current next])

; A Presentation is one of:
; - #false
; - (make-presentation Slide Presentation)
; Interpretation: the slides in the presentation,
; or #false if it is empty
(define Presentation-1 #false)
(define Presentation-2 (make-presentation SLIDE-1 Presentation-1))
(define Presentation-3 (make-presentation SLIDE-2 Presentation-2))
(define Presentation-4 (make-presentation SLIDE-3 Presentation-3))
(define Presentation-5 (make-presentation SLIDE-4 Presentation-4))
(define (presentation-temp p)
  (...
   (cond
     [(boolean? p) ...]
     [(presentation? p) ...
      (slide-temp (presentation-current p)) ...
      (presentation-temp(presentation-next p)) ...])))

; count-slides: Presentation-> Number
; return the number of slides which uses animations
(check-expect (count-slides Presentation-1) 0)
(check-expect (count-slides Presentation-2) 1)
(check-expect (count-slides Presentation-3) 1)
(check-expect (count-slides Presentation-4) 1)
(check-expect (count-slides Presentation-5) 2)
(define (count-slides p)
  (cond
    [(boolean? p) 0]
    [(presentation? p) 
     (+ (count (presentation-current p)) 
        (count-slides(presentation-next p)))]))
; count: slide-> Number
; if slide uses animation, return 1,
; otherwise return 0
(check-expect (count SLIDE-1) 1)
(check-expect (count SLIDE-2) 0)
(define (count c)
  (if (boolean=? (slide-animations? c) #true) 1 0))

; (a) Finish the Data Design Recipe for Presentation


; (b) Design a function count-slides that accepts a Presentation and returns
; the number of slides that uses animations. Be sure to show all steps of the
; design recipe. You may use your examples from part (a) if you wish :)



; HINT : Parts (c), (d), and (e) work very well together :)


; (c) When slides are submitted for evaluation, Professor Derbinsky also needs to submit
; how long it takes for him to present them. Design a function total-time that accepts
; a Presentation and returns the total time it takes for him to present the slides.
; Be sure to show all the steps of the design recipe. You may use your examples from
; part (a) if you wish.

; total-time: Presentation-> Number
; returns the total time to show the slides
(check-expect (total-time Presentation-1) 0)
(check-expect (total-time Presentation-2) 20)
(check-expect (total-time Presentation-3) 50)
(check-expect (total-time Presentation-4) 90)
(check-expect (total-time Presentation-5) 110)
(define (total-time p)
  (cond
    [(boolean? p) 0]
    [(presentation? p) 
     (+ (time (presentation-current p))
        (total-time(presentation-next p)))]))
; time: Slide-> Number
; returns the time of slide
(check-expect (time SLIDE-1)20)
(check-expect (time SLIDE-2)30)
(check-expect (time SLIDE-3)40)
(check-expect (time SLIDE-4)20)
(define (time s)
  (slide-time s))



; (d) When Professor Derbinsky teaches a class, he has to make sure his Presentations
; don't go over the time allotted for that class period. Design the function has-time?
; that accepts a time limit for the class period and a Presentation; it then returns
; #true if his presentation does not go over the time allotted for the class. Be sure
; to show all steps of the design recipe. You may use your examples from
; part (a) if you wish.

; has-time?: Number Presentation->Boolean
; determine the presentation over class period
(check-expect (has-time? 10 Presentation-1) #true)
(check-expect (has-time? 40 Presentation-3) #false)
(check-expect (has-time? 100 Presentation-4) #true)
(define (has-time? t p)
  (cond
    [(boolean? p) #true ]
    [(presentation? p)
     (> t (total-time p))]))


; (e) If the presentation goes over the allotted time, Professor Derbinsky needs to know
; by how much his presentation goes over. Design the function time-over that
; accepts a Presentation and a time limit for the class period; it then returns
; the amount of time that his presentation goes over the time allotted, or 0 if the
; presentation does not go over the time allotted. Be sure to show all steps of the
; design recipe. You may use your examples from part (a) and function from part (b).

; time-over: Number Presentation-> Number
; find the time to finish the presentation
; if it goes over the class period
(check-expect (time-over 10 Presentation-1) 0)
(check-expect (time-over 40 Presentation-3) 10)
(check-expect (time-over 100 Presentation-4) 0)
(define (time-over t p)
  (if (has-time? t p)
      0
      (- (total-time p) t)))


