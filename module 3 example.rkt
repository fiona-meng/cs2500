;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m3_l2_v3p2_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Design a function passed? that determines if selecting
; the same response for all five questions on a
; True/False Test gets you a passing grade
; (at least 3-out-of-5). Each question has a prompt and
; a correct answer.

(define-struct tfq [prompt correct])

; A TFQuestion is a (make-tfq String Boolean)
; Interpretation: a true/false question
; - prompt is the question being asked
; - correct is the correct response

(define TFQ-1
  (make-tfq "Northeastern has a campus in Hawaii" #false))

(define TFQ-2
  (make-tfq "Northeastern has a campus in London" #true))

(define TFQ-3
  (make-tfq "Northeastern has a campus in Seattle" #true))

(define TFQ-4
  (make-tfq "Northeastern has a campus in Florida" #false))

(define TFQ-5
  (make-tfq "Northeastern has a campus in Egypt" #false))

(define (tfq-temp tfq)
  (... (tfq-prompt tfq) ...
       (tfq-correct tfq) ...))

(define-struct tft [q1 q2 q3 q4 q5])

; A TFTest is a (make-tft TFQuestion TFQuestion TFQuestion TFQuestion TFQuestion)
; Interpretation: a 5-question T/F test

(define TFT-1
  (make-tft TFQ-1 TFQ-2 TFQ-3 TFQ-4 TFQ-5))

(define (tft-temp tft)
  (... (tfq-temp (tft-q1 tft)) ...
       (tfq-temp (tft-q2 tft)) ...
       (tfq-temp (tft-q3 tft)) ...
       (tfq-temp (tft-q4 tft)) ...
       (tfq-temp (tft-q5 tft)) ...))

; passed? : TFTest Boolean -> Boolean
; if the supplied answer is used on all
;questiona of the supplied test, would
; a score of at least 3/5 result?
(check-expect (passed? TFT-1 #true) #false)
(check-expect (passed? TFT-1 #false) #true)

(define (passed? tft ans)
  (>=
   (+
    (score (tft-q1 tft)ans)
    (score (tft-q2 tft)ans)
    (score (tft-q3 tft)ans)
    (score (tft-q4 tft)ans)
    (score (tft-q5 tft)ans))
    3))
; score: TFQuestion Boolean ->(0,1)
;returns 0 if incorrect answer supplied,
;otherwise 1
(check-expect (score TFQ-1 #true) 0)
(check-expect (score TFQ-1 #false) 1)
  
(define (score tfq ans)
  (if
   (boolean=? (tfq-correct tfq) ans)
   1
   0
  ))











