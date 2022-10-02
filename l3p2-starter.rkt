;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname l3p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Lab 3, Problem 2 ==

; You are to design a program text-mover
; to display and manipulate text on a
; background. Your program should accept
; some phrase to show, as well as initial
; location and color (we only support three:
; red, black, or purple) - you should then
; display the phrase on the screen as described.

; When the user presses a mouse
; button, the program should move the text
; to the location that they clicked. When
; the user presses a key on the keyboard,
; the program should rotate colors.

; TODO #1: design the text-mover function;
; think through the arguments to the function,
; how you will represent the world state,
; and what handlers you need to support.
; - Hint A: since your state has multiple parts
;           that change, you'll need a structure
;           to hold them, but the parts themselves
;           might also be new.
; - Hint B: you've been provided some data definitions
;           below that will be quite useful

; TODO #2: Finish designing the data from #1;
; think ahead to make examples that are useful
; for testing such operations as changing location
; and color

; TODO #3: design your to-draw handler, making
; use of the template(s) you designed in #2.

; TODO #4: design your remaining handler(s),
; again following the appropriate template(s).
; - Hint #1: for the mouse, you'll want
;            to respond only to the "button-up"
;            event, which you can check using
;            the mouse=? function.
; - Hint #2: make sure to follow your templates,
;            which may involve breaking the handlers
;            into helper functions.

(define BACKGROUND (empty-scene 600 400))

; A Position is a (make-posn Real Real)
; Interpretation: a 2D location
(define posn-1 (make-posn 0 0))
(define posn-2 (make-posn 100 100))
(define (posn-temp p)
  (... (posn p)...))

; A RedBlackPurple (RBP) is one of:
; - "red"
; - "black"
; - "purple"
; Interpretation: available font colors
(define RBP-RED "red")
(define RBP-BLACK "black")
(define RBP-PURPLE "purple")
(define (rbp-temp r)
  (... (RBP-RED) ...
   ... (RBP-BLACK) ...
   ... (RBP-PURPLE) ...))
       

(define-struct tm [str pos col])

; A TextMover (TM) is a (make-tm String Position RBP)
; - str is the text to be displayed
; - pos is the location of the text
; - col is the color of the text
; Interpretation: all the information needed for
; the text-mover appliction
(define TM-1 (make-tm "Hello" posn-2 RBP-RED))
(define (tm-temp t)
  (... (tm-str t) ...
       (tm-pos t) ...
       (tm-col t) ... ))
       

;text-mover: TextMover-> TextMover
;manipulate text and change color
(define (text-mover inital-tm)
  (big-bang inital-tm
    [to-draw draw-tm]
    [on-key color-tm]
    [on-mouse pos-tm]
    ))

; draw-tm: TextMover-> TextMover
; draw text on the background
(check-expect (draw-tm TM-1)
              (place-image (text "Hello" 24 RBP-RED) 100 100 BACKGROUND))
(define (draw-tm tm)
  (place-image (text (tm-str tm) 24 (tm-col tm)) (tm-pos tm) BACKGROUND))

; color-tm: TextMover KeyEvent-> TextMover
; change text color

(define (color-tm tm)
  (cond
    [(key=? tm RBP-RED) RBP-BLACK]
    [(key=? tm RBP-BLACK)RBP-PURPLE]
    [(key=? tm RBP-PURPLE) RBP-RED]))
     
    


; pos-tm: TextMover-> TextMover
; change position
(define (pos-tm tm)
  (if
   (mouse=? "button-up")
   (place-image (text (tm-str tm) 24 (tm-col tm)) "mouse-click-posn" BACKGROUND)
   (place-image (text (tm-str tm) 24 (tm-col tm)) (tm-pos tm) BACKGROUND)))

(define (mouse-click-posn mouse x y dx dy))
  

 
  


