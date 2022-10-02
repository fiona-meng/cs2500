;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
; == Homework 3, Problem 2 ==

; TODO: design the function command-point that accepts a command
; ("left", "right", "up", "down"), an (x, y) position,
; and a non-negative distance and produces a new point moving
; according to the command and distance. For example, if you
; start at (2, 3), the command "left" 1 should produce (1, 3),
; whereas "down" 2.2 would produce (2, 5.2).
;
; You should design whatever data is necessary for this function
; and, as always, remember to follow the templates in your function
; implementation (hint: if your command-point function is getting
; large and repetitive, you are likely not following the templates
; and need a helper).

; A position is a (make-posn Real Real)
; Interpretation:a 2D point
(define posn-1 (make-posn 5 10))
(define posn-2 (make-posn 6 10))
(define posn-3 (make-posn 4 10))
(define posn-4 (make-posn 5 9))
(define posn-5 (make-posn 5 11))
(define (posn-temp p)
  (... (posn p)
       (posn p) ...))
; move: Position KeyEvent -> Position
; move the position by pressing the key
(check-expect (move posn-1 "right") posn-2)
(check-expect (move posn-1 "left") posn-3)
(check-expect (move posn-1 "up") posn-4)
(check-expect (move posn-1 "down") posn-5)
(check-expect (move posn-1 "a") posn-1)
(define (move p key)
  (cond
    [(key=? key "right") (move-pos p 1 0)]
    [(key=? key "left") (move-pos p -1 0)]
    [(key=? key "up") (move-pos p 0 -1)]
    [(key=? key "down") (move-pos p 0 1)]
    [else p]))

; move-pos: Position Real Real -> Position
; change the position by the supplied
; changes in x and y coordinate
(check-expect (move-pos posn-1 1 0)
              (make-posn 6 10))
(define (move-pos p dx dy)
  (make-posn (+ (posn-x p) dx)
             (+ (posn-y p) dy)))

; command-point: Position -> Position
; allow ueser to produce a new position by pressing the key

(define (command-point inital)
  (big-bang inital
    [to-draw draw]
    [on-key move]))
; draw: Position -> Image
; display the position
(check-expect (draw posn-1)
              (place-image
               (text (string-append "("
                                    (number->string(posn-x posn-1))
                                    ","
                                    (number->string(posn-y posn-1))
                                    ")")
                     24
                     "olive")
               150
               150
               (square 300 "solid" "white")))
                                    
(define (draw p)
  (place-image
   (text (string-append "("
                        (number->string(posn-x p))
                        ","
                        (number->string(posn-y p))
                        ")")
         24
         "olive")
   150
   150
   (square 300 "solid" "white")))
   


    

