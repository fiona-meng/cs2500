;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Exam 1, Problem 3 ==

; Consider the following data definitions:


(define-struct helped [name waited help-length])

; A HelpedStudent (HS) is a (make-helped String NaturalNumber NaturalNumber)
; Interpretation: a helped student
; - name is the student's name
; - waited is how many minutes they waited before being helped
; - help-length is how many minutes the student was helped

(define HS-1 (make-helped "anna" 3 10))
(define HS-2 (make-helped "daniel" 12 4))


(define-struct waiting [name minutes])

; A WaitingStudent (WS) is a (make-waiting String NaturalNumber)
; Interpretation: a student waiting for help
; - name is the student's name
; - minutes is the time they've been waiting so far

(define WS-1 (make-waiting "anushka" 8))
(define WS-2 (make-waiting "damon" 3))


; A StudentQuestion(SQ) is one of:
; - HelpedStudent
; - WaitingStudent
; Interpretation: a student in the help queue
; that has either been helped or is waiting
(define SQ-1 HS-1)
(define SQ-2 HS-2)
(define SQ-3 WS-1)
(define SQ-4 WS-2)


(define-struct hq [student next])

; A HelpQueue (HQ) is one of:
; - #false
; - (make-hq StudentQuestion HelpQueue)
; Interpretation: a help queue, or #false if
; it's empty
(define HQ-1 #false)
(define HQ-2 (make-hq SQ-1 HQ-1))
(define HQ-3 (make-hq SQ-2 HQ-2))
(define HQ-4 (make-hq SQ-3 HQ-3))
(define HQ-5 (make-hq SQ-4 HQ-4))


; TODO #1: provide examples of HelpQueue using
; each of the helped/waiting examples provided
; exactly once (but in any order you choose).



; TODO #2: design the function queue-wait that accepts
; a HelpQueue and returns the total number of minutes
; that students have waited in the queue for those that
; have not yet been helped. For example, in a queue that
; includes all the examples above, the wait would be 11,
; because anushka and damon haven't been helped (sorry!),
; so 3 + 8 = 11.

; queue-wait: HelpQueue-> Number
; return the mintues which students need to wait for helping
(check-expect (queue-wait HQ-1) 0)
(check-expect (queue-wait HQ-2) 0)
(check-expect (queue-wait HQ-3) 0)
(check-expect (queue-wait HQ-4) 8)
(check-expect (queue-wait HQ-5) 11)
(define (queue-wait q)
  (cond
    [(boolean? q) 0]
    [(hq? q) (+ (help (hq-student q)) (queue-wait (hq-next q)))]))
; help: StudentQuestion-> Number
; determine the minutes students need to wait
(check-expect (help SQ-1) 0)
(check-expect (help SQ-2) 0)
(check-expect (help SQ-3) 8)
(check-expect (help SQ-4) 3)
(define (help h)
  (cond
    [(helped? h) 0]
    [(waiting? h) (waiting-minutes h)]))

     
; You do not have to write templates for any of the data,
; however, we will expect that you follow the correct
; templates for these definitions (and will deduct credit
; accordingly).


