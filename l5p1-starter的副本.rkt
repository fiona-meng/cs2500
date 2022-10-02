;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname l5p1-starter的副本) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Lab 5, Problem 1 ==

; Your goal is to design a slide show program. Every
; slide will have a title and a list of bullets to
; display, and each slide and item will be preceded
; by a key press.

; Consider the data definitions and examples below...

; A ListOfStrings (LoS) is one of:
; - '()
; - (cons String LoS)
; Interpretation: a list of strings

(define SLIDE-1-LOS
  (cons "Designing programs to solve problems"
        (cons "Building good habits for developing large systems with a team"
              (cons "CS: fundamental ideas, thinking" '()))))

(define SLIDE-2-LOS
  (cons "Effectively using computers as tools"
        (cons "Breaking down problems" '())))

(define SLIDE-3-LOS
  (cons "Easy to start"
        (cons "Informative feedback"
              (cons "Functional programming is a useful paradigm" '()))))
(define (los-temp los)
  (...
   (cond
     [(empty? los) ...]
     [(cons? los) ...
      (first los) ...
      (los-temp(rest los)) ...])))


(define-struct slide [title shown hidden])

; A Slide is a (make-slide String LoS LoS)
; Interpretation: a slide's title, what bullets
; have been shown, and those that are hidden

(define SLIDE-1
  (make-slide
   "What is Fundies 1 About?"
   '() SLIDE-1-LOS))

(define SLIDE-1-NEXT
  (make-slide
   "What is Fundies 1 About?"
   (cons "Designing programs to solve problems" '())
   (cons "Building good habits for developing large systems with a team"
         (cons "CS: fundamental ideas, thinking" '()))))

(define SLIDE-1-NEXT-NEXT
  (make-slide
   "What is Fundies 1 About?"
   (cons "Designing programs to solve problems"
         (cons "Building good habits for developing large systems with a team" '()))
   (cons "CS: fundamental ideas, thinking" '())))

(define SLIDE-1-DONE
  (make-slide "What is Fundies 1 About?"
              SLIDE-1-LOS '()))

(define SLIDE-2
  (make-slide "What is Computer Science?"
              '() SLIDE-2-LOS))

(define SLIDE-3
  (make-slide "Why DrRacket?"
              '() SLIDE-3-LOS))
(define (slide-temp s)
  (... (slide-title s) ...
       (los-temp(slide-shown s)) ...
       (los-temp(slide-hidden s)) ...))


; A Slideshow is one of:
; - '()
; - (cons Slide Slideshow)
; Interpretation: an ordered slideshow

(define SLIDESHOW-1
  (cons SLIDE-1 (cons SLIDE-2 (cons SLIDE-3 '()))))

(define SLIDESHOW-1-NEXT
  (cons SLIDE-1-NEXT (cons SLIDE-2 (cons SLIDE-3 '()))))

(define SLIDESHOW-1-DONE
  (cons SLIDE-1-DONE (cons SLIDE-2 (cons SLIDE-3 '()))))

(define SLIDESHOW-2
  (cons SLIDE-2 (cons SLIDE-3 '())))

(define (Slideshow-temp s)
  (...
   (cond
     [(empty? s) ...]
     [(cons? s) ...
      (slide-temp(first s)) ...
      (Slideshow-temp(rest s)) ...])))

(define Background(square 500 "solid" "white"))

; draw-slide: Slide -> Image
; visualizes a system of slide

(check-expect (draw-slide SLIDE-2)
              (place-image
               (above (text "What is Computer Science?" 24 "purple")
                      (text " " 24 "white"))
               250 250 Background))

(define (draw-slide s)
  (place-image
   (above (text (slide-title s) 24 "purple")
          (draw-shown (slide-shown s)))
   250 250 Background))


; draw-shown: LoS -> Imgae
; draws the shown part
(check-expect (draw-shown SLIDE-2-LOS)
              (above (text "Effectively using computers as tools" 24 "purple")
                     (above (text "Breaking down problems" 24 "purple")
                            (text " " 24 "white"))))
(define (draw-shown los)
  (cond
    [(empty? los) (text " " 24 "white") ]
    [(cons? los) 
     (above (text (first los) 24 "purple")
            (draw-shown (rest los)))]))


; draw-slideshow: Slideshow -> Image
; visualize slideshow
(check-expect (draw-slideshow SLIDESHOW-2)
              (draw-slide SLIDE-2))
(define (draw-slideshow s)
  (cond
    [(empty? s) (text "Fin" 24 "purple")]
    [(cons? s) 
     (draw-slide(first s))]))

; advance-slide: Slide -> Slide
; Visualize slide by parts
(check-expect (advance-slide SLIDE-1) SLIDE-1-NEXT)
(define (advance-slide s)
  (make-slide (slide-title s)
              (shown s)
              (hidden s)))
; shown: Slide -> LoS
; add one more shown content
(check-expect (shown SLIDE-1) (cons "Designing programs to solve problems" '() ))
(check-expect (shown SLIDE-1-NEXT)
              (cons "Designing programs to solve problems"
                    (cons "Building good habits for developing large systems with a team" '())))
(check-expect (shown SLIDE-1-DONE)
              (cons "Designing programs to solve problems"
                    (cons "Building good habits for developing large systems with a team"
                          (cons "CS: fundamental ideas, thinking" '()))))
(define (shown los)
  (cond
    [(empty? (slide-shown los)) (cons (first (slide-hidden los)) '())]
    [(cons? (slide-shown los)) 
     (append
      (slide-shown los)
      (if (empty? (slide-hidden los))
          '()
          (cons (first (slide-hidden los)) '())))]))
; hidden: Slide -> LoS
; reduce content in the hidden part
(check-expect (hidden SLIDE-1)
              (cons "Building good habits for developing large systems with a team"
                    (cons "CS: fundamental ideas, thinking" '())))
(check-expect (hidden SLIDE-1-DONE) '())
(define (hidden los)
  (cond
    [(empty? (slide-hidden los)) '()]
    [(cons? (slide-hidden los))
     (rest (slide-hidden los))]))
; slide-over?: Slide -> Boolean
; detemine whether the slide is over
(check-expect (slide-over? SLIDE-1-DONE) #true)
(check-expect (slide-over? SLIDE-1) #false)
(define (slide-over? s)
  (if (empty? (slide-hidden s))
      #true #false))


; advance-slideshow: Slideshow -> Image
; visualize the advance-slideshow
(define (advance-slideshow los)
  (cond
     [(slide-over? los) (advance-slide los)]
     [(cons? los) 
      (draw-shown (first los))]))
; go-slideshow: Slideshow -> Image
(define (go-slideshow inital)
  (big-bang inital
    [to-draw advance-slideshow]
    [on-key begin]))

; begin: KeyEvent -> Imgae
; Start the fucntion
(define (begin los ke)
  (advance-slideshow ke))

 
     
; TODO #1: Complete the Design Recipe for LoS, Slide
; and Slideshow by creating their templates.


; TODO #2: Design the function draw-slide that draws
; a slide, showing only its title and unhidden content
; on a large background of a fixed size. The text of
; the bullets should be arranged above each other.
; You are free to be creative about your slide design :)


; TODO #3: Design the function draw-slideshow that
; draws the slideshow’s first slide; if the slideshow
; is complete, you should show "Fin" as the title to
; an empty slide. Either way, this visualization should
; be placed on a large background of fixed size.


; TODO #4: Design the function advance-slide that
; moves the first entry in a slide’s hidden content
; to the end of its shown content if there is any
; hidden content. As examples, look to SLIDE-1 ->
; SLIDE-1-NEXT -> SLIDE-1-NEXT-NEXT -> SLIDE-1-DONE.
; (Hint: the append function will be quite useful.)


; TODO #5: Design the function slide-over? that
; determines if a slide is over (none of its bullets
; are hidden).


; TODO #6: Design the function advance-slideshow that
; either advances its first slide if it has more content
; to be shown or moves onto the next slide if there
; is one.



; TODO #7: Design the World program go-slideshow
; that will advance a slideshow when any key is
; pressed. The program should end (i.e., stop-when)
; when there are no more slides left, showing
; our elegant "Fin" as the last image.
   
