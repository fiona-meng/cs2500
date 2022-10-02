;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1review-p1-sample) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
       (slide-content s)))


(define-struct presentation [current next])

; A Presentation is one of:
; - #false
; - (make-presentation Slide Presentation)
; Interpretation: the slides in the presentation,
; or #false if it is empty



; (a) Finish the Data Design Recipe for Presentation

(define P-1 #false)
(define P-2 (make-presentation SLIDE-1 P-1))
(define P-3 (make-presentation SLIDE-2 P-2))
(define P-4 (make-presentation SLIDE-3 P-3))
(define P-5 (make-presentation SLIDE-4 P-4))

(define (pres-temp p)
  (...
   (cond
     [(boolean? p) ...]
     [(presentation? p) ...
      (... (slide-temp (presentation-current p))...
           (pres-temp (presentation-next p)) ...)])))



; (b) Design a function count-slides that accepts a Presentation and returns
; the number of slides that uses animations. Be sure to show all steps of the
; design recipe. You may use your examples from part (a) if you wish :)

; count-slides : Presentation -> NaturalNumber
; outputs the number of slides that uses animations

(check-expect (count-slides P-1) 0)
(check-expect (count-slides P-2) 1)
(check-expect (count-slides P-3) 1)
(check-expect (count-slides P-4) 1)
(check-expect (count-slides P-5) 2)

(define (count-slides p)
  (cond
    [(boolean? p) 0]
    [(presentation? p)
     (+ (if (slide-animations? (presentation-current p)) 1 0)
        (count-slides (presentation-next p)))]))



; HINT : Parts (c), (d), and (e) work very well together :)

; (c) When slides are submitted for evaluation, Professor Derbinsky also needs to submit
; how long it takes for him to present them. Design a function total-time that accepts
; a Presentation and returns the total time it takes for him to present the slides.
; Be sure to show all the steps of the design recipe. You may use your examples from
; part (a) if you wish.

; total-time : Presentation -> NaturalNumber
; returns the total time it takes Professor Derbinsky to present

(check-expect (total-time P-1) 0)
(check-expect (total-time P-2) 20)
(check-expect (total-time P-3) 50)
(check-expect (total-time P-4) 90)
(check-expect (total-time P-5) 110)

(define (total-time p)
  (cond
    [(boolean? p) 0]
    [(presentation? p)
     (+ (slide-time (presentation-current p))
        (total-time (presentation-next p)))]))



; (d) When Professor Derbinsky teaches a class, he has to make sure his Presentations
; don't go over the time allotted for that class period. Design the function has-time?
; that accepts a time limit for the class period and a Presentation; it then returns
; #true if his presentation does not go over the time allotted for the class. Be sure
; to show all steps of the design recipe. You may use your examples from
; part (a) if you wish.

; has-time? : Presentation NaturalNumber -> Boolean
; returns true if Professor Derbinsky has enough time for his presentation

(check-expect (has-time? P-1 10) #true)
(check-expect (has-time? P-2 20) #true)
(check-expect (has-time? P-3 10) #false)
(check-expect (has-time? P-4 115) #true)
(check-expect (has-time? P-5 109) #false)

(define (has-time? p n)
  (cond
    [(boolean? p) #true]
    [(presentation? p)
     (<= (total-time p) n)]))



; (e) If the presentation goes over the allotted time, Professor Derbinsky needs to know
; by how much his presentation goes over. Design the function time-over that
; accepts a Presentation and a time limit for the class period; it then returns
; the amount of time that his presentation goes over the time allotted, or 0 if the
; presentation does not go over the time allotted. Be sure to show all steps of the
; design recipe. You may use your examples from part (a) and function from part (b).

; time-over : Presentation NaturalNumber-> NaturalNumber
; returns the amount of time that the presentation goes over
; or 0 if it doesn't go over

(check-expect (time-over P-1 10) 0)
(check-expect (time-over P-2 20) 0)
(check-expect (time-over P-3 40) 10)
(check-expect (time-over P-4 115) 0)
(check-expect (time-over P-5 109) 1)

(define (time-over p n)
  (cond
    [(boolean? p) 0]
    [(presentation? p)
     (if (has-time? p n)
         0
         (- (total-time p) n))]))

