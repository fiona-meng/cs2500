;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab1 for P2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define HOUSE-WITH-WINDOWS(square 40 200 "blue"))
(define Roof (triangle 150 100 "green"))
(define Basic (overlay/xy
               (overlay/xy(square 150 100 "yellow")
                          20
                          30
                          HOUSE-WITH-WINDOWS)
               90
               30
               HOUSE-WITH-WINDOWS))
                         
(define Background (above Roof Basic))
(define Door (overlay/align "right" "middle" (circle 7 "solid" "pink")(rectangle 50 90 200 "red")))
(define House(place-image Door 75 250 Background))


(define (sky-color time)
  (place-image House 150 150 (square 300 time (make-color 0 0 (abs(-(remainder time 510) 255))))))







  