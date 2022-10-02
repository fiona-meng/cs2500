;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 4, Problem 3 ==

; Consider the following data definitions:

; A Genre is one of:
; - "comedy"
; - "drama"
; - "action"
; - "education"
; Interpretation: genre for a video
(define G-C "comedy")
(define G-D "drama")
(define G-A "action")
(define G-E "Education")
(define (Genre-temp g)
  (...
   (cond
     [(string=? g G-C) ...]
     [(string=? g G-D) ...]
     [(string=? g G-A) ...]
     [(string=? g G-E) ...])))
     
(define-struct video [name duration hd? genre next])

; A StreamingQueue(SQ) is one of:
; - #false
; - (make-video String PosInteger Boolean Genre StreamingQueue)
; Interpretation: either an empty queue
; or a video with a name, duration in minutes,
; whether it's available in HD, and it's genre.
(define SQ-NONE #false)
(define SQ-1 (make-video "Tenet" 150 "#false" G-A SQ-NONE))
(define SQ-2 (make-video "How to Train Your Dragon" 9 "#true" G-C SQ-1))
(define (SQ-temp s)
  (...
   (cond
     [(boolean? s) ...]
     [(video? s) ...
      ... (video-name s) ...
      ... (video-duration s) ...
      ... (video-hd? s) ...
      ... (Genre-temp (video-genre s)) ...
      ... (SQ-temp (video-next s)) ...])))
; TODO #1: complete the Design Recipe for Genre
; and StreamingQueue
; TODO #2: design the function queue-pic that produces
; an image of each title (with its duration) in the queue
; stacked vertically.
(define Background(square 100 "solid" "white"))
; draw: video -> Image
; draw title and duration
(check-expect (draw SQ-1)
              (text (string-append "Tenet" " (" (number->string 150) " min)") 10 "purple"))
(define (draw s)
  (text (string-append (video-name s) " (" (number->string(video-duration s)) " min)") 10 "purple"))

; queue-pic: SQ->Image
; visualize name and duration of video
(check-expect (queue-pic SQ-1)
              (above
               (text (string-append "Tenet" " (" (number->string 150) " min)") 10 "purple")
               (text " " 10 "purple")))
(define (queue-pic s)
  (cond
    [(boolean? s) (text " " 10 "purple")]
    [(video? s)
     (above (draw s) (queue-pic (video-next s)))]))

; TODO #3: design the function all-hd? that determines
; if all the videos in the queue are available in HD.

; all-hd?: Video-> Boolean
; determine whether all listed videos are avaliable
(check-expect (all-hd? SQ-1) #false)
(check-expect (all-hd? SQ-2) #true)
(define (all-hd? s)
  (equal? (video-hd? s) "#true"))

; TODO #4: design the function only-short that takes a
; queue and returns a new queue with only those videos
; in the original that are at most 12 minutes.
; only-short: SQ -> Image
; retunrs the video which are at most 12 minutes
(check-expect (only-short SQ-2)
              (above (text (string-append
                            "How to Train Your Dragon" " (" (number->string 9) " min)") 10 "purple")
                     (text " " 10 "purple")))
(check-expect (only-short SQ-NONE) (text " " 10 "purple"))
(define (only-short s)
  (cond
    [(boolean? s) (text " " 10 "purple")]
    [(video? s)
     (if (<(video-duration s) 12) 
         (above (draw s) (only-short (video-next s)))
         (text " " 10 "purple"))]))
; TODO #5: design the function add-ad-time that adds 1 minute
; to the duration of every video to account for requisite ads.
; add-ad-time: SQ-> Image
; visualize the add  1 minute to the duration
(check-expect (add-ad-time SQ-1)
              (above (text "Tenet (151min) #false action" 10  "purple")
                     (text " " 10 "purple")))

(define (add-ad-time s)
  (cond
    [(boolean? s) (text " " 10 "purple")]
    [(video? s)
     (above (text (add-time s)10 "purple")
            (add-ad-time (video-next s)))]))
; add-time: SQ-> String
; add 1 min to the duration of video as ad
(check-expect (add-time SQ-1)
              "Tenet (151min) #false action")
             
(define (add-time s)
  (string-append (video-name s) " ("
                 (number->string (+ (video-duration s) 1)) "min) "
                 (video-hd? s) " "
                 (video-genre s)))

; TODO #6: design the function any-funny? that determines if any
; video in the queue is in the comedy genre.
; any-funny?: SQ->Image
; visualize comdey genre
(check-expect (any-funny? SQ-2)
              (above(text "How to Train Your Dragon" 10 "purple")
                    (above (text " " 10 "purple")
                           (text " " 10 "purple"))))
(define (any-funny? s)
  (cond
    [(boolean? s) (text " " 10 "purple")]
    [(video? s)
     (above (text (Genre s) 10 "purple")
            (any-funny? (video-next s)))]))
; Genre: Genre->String
; find the comedy drama
(check-expect (Genre SQ-2)
              "How to Train Your Dragon")
(check-expect (Genre SQ-1) " ")
(define (Genre g)
  (if (equal? (video-genre g) G-C) (video-name g) " "))



