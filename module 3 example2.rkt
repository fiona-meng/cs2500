;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |module 3 example2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Design a World program collision simulating two cars driving
; at each other. One starts to the left, and drives right;
; the other starts to the right and drives left. They each
; start with their own velocity but both accelerate at the
; same rate (0.1 pixels/tick per tick).

(define-struct twocars [x1 vx1 x2 vx2])

; A TwoCars is a (make-twocars Real Real Real Real)
; Interpretation: two cars driving at each other
; - x1 is position of the first car in pixels from the left
; - vx1 is the velocity in pixels/tick driving from the left
; - x2 is position of the second car in pixels from the left
; - vx2 is the velocity in pixels/tick driving from the right

(define TC-1 (make-twocars 2 1 500 2))
(define TC-2 (make-twocars 3 1.1 498 2.1))

(define (tc-temp tc)
  (... (twocars-x1 tc) ...
       (twocars-vx1 tc) ...
       (twocars-x2 tc) ...
       (twocars-vx2 tc) ...))

; collision : TwoCars -> TwoCars
; simulates two cars cpllliding
(define (collision inital-tc)
  (big-bang inital-tc
    [to-draw draw-tc]
    [on-tick move-tc]))

; draw-tc: TwoCars -> Image
; draw two cars on the background


(define CAR-1 (rectangle 10 5 "solid" "blue"))
(define CAR-2 (rectangle 10 5 "solid" "red"))
(define Y-CAR 50)
(define BACKGROUND (empty-scene 600 400))

(check-expect (draw-tc TC-1)
              (place-image CAR-1 2 Y-CAR
                           (place-image CAR-2 500 Y-CAR BACKGROUND)))
(define (draw-tc tc)
  (place-image CAR-1 (twocars-x1 tc) Y-CAR
               (place-image CAR-2 (twocars-x2 tc) Y-CAR BACKGROUND)))

; move-tc: TwoCars-> TwoCars
; moves and applies acceleration
; to the cars as a tick passes
(check-expect (move-tc TC-1) TC-2)
(define (move-tc tc)
  (make-twocars (+ (twocars-x1 tc) (twocars-vx1 tc))
                (+ (twocars-vx1 tc) ACCEL )
                (- (twocars-x2 tc) (twocars-vx2 tc))
                (+ (twocars-vx2 tc) ACCEL )))

(define ACCEL 0.1)

