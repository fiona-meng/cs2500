;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw10p1-starter (2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 10, Problem 1 ==

; Consider a gradebook for a class.

; In order to represent how individual assignments lead to a class
; grade, you can think that there are assignment columns (each
; with a name and total points) and calculated columns (each with
; a name, operation, and columns over which to operate).
; Operations include taking the simple average, dropping some
; number of lowest values, or weighting the columns.

; For example...
; - a total column, representing a weighted average over...
;   - a homework column (worth 30% of total), representing a simple average over...
;     - 4 assignment columns (hw1-4; each out of 20pts)
;   - a project column (out of 100pts; worth 50% of total)
;   - a quizzes column (worth 20% of total), representing a weighted average over...
;     - a pre-class quizzes column (worth 20% of quizzes; with the lowest dropped), broken into...
;       - 3 columns (pcq1-3; each out of 5pts)
;     - an in-class quizzes column (worth 80% of quizzes; with the lowest dropped), broken into...
;       - 3 columns (icq1-3; each out of 10pts)


; TODO #1: using the above description, design the data for a Gradebook.
; You should represent the gradebook description above with your examples.

;???
;(define TOTAL-COLUMN (list HOMEWORK PROJECT QUIZZES))
;(define HOMEWORK CAL-1)
;(define PROJECT AS-5)
;(define QUIZZES (list PRE-CLASS-Q IN-CLASS-Q))
;(define PRE-CLASS-Q CAL-2)
;(define IN-CLASS-Q CAL-3)

; A Operation is one of:
; - "average"
; - Number
; - [List-of Number]
; Interpretation: an operation is one of
; - taking the average number when given a string
; - droping the lowest grades when given how many lowest grades to drop
; - and weighting the column when given a list of grades

(define OP-1 "average")
(define OP-2 1)
(define OP-3 (list 30 20 50))
(define OP-4 (list 20 80))

(define (operation-temp o)
  (...(string? o)...
      (number? o)...
      (list? o)...))

(define-struct assignment [name total-points])

; A Assignment is (make-assignment String Number)
; Interpretation: an assignment column with the name and the score

(define AS-0 (make-assignment "" 0))
(define AS-1 (make-assignment "Assignemnt-1" 20))
(define AS-2 (make-assignment "Assignemnt-2" 20))
(define AS-3 (make-assignment "Assignemnt-3" 20))
(define AS-4 (make-assignment "Assignemnt-4" 20))
(define AS-5 (make-assignment "Project" 100))
(define AS-6 (make-assignment "Pre-class-quiz-1" 5))
(define AS-7 (make-assignment "Pre-class-quiz-2" 5))
(define AS-8 (make-assignment "Pre-class-quiz-3" 5))
(define AS-9 (make-assignment "In-class-quiz-1" 10))
(define AS-10 (make-assignment "In-class-quiz-2" 10))
(define AS-11 (make-assignment "In-class-quiz-3" 10))

(define (assignment-temp a)
  (...(assignment-name a)...
      (assignment-tp a)...))


(define-struct calculated [name operation column])

; A Calculated is (make-calculated String Operation [List-of Gradebook])
; Interpretation: a calculated column with the name, the operation
; and the column to operate

(define CAL-1 (make-calculated "Homework" OP-1 (list AS-1 AS-2 AS-3 AS-4)))
(define CAL-2 (make-calculated "Project" OP-1 (list AS-5)))
(define CAL-3 (make-calculated "Pre-class-quizzes" OP-2 (list AS-6 AS-7 AS-8)))
(define CAL-4 (make-calculated "In-class-quizzes" OP-2 (list AS-9 AS-10 AS-11)))
(define CAL-5 (make-calculated "quizzes" OP-4 (list CAL-3 CAL-4)))
(define CAL-6 (make-calculated "total" OP-3 (list CAL-1 CAL-2 CAL-5)))

(define (calculated-temp c)
  (...(caculated-name c)...
      (operation-temp (caculated-operation c))...
      (log-temp (caculated-column c))...))

(define (log-temp log)
  (...
   (cond
     [(empty? log)...]
     [(cons? log)...
      (gradebook-temp (first log))...
      (log-temp (rest log))...])))


; A Gradebook is one of:
; - (make-assignment String Number)
; - (make-calculated String Operation [List-of Gradebook])
; Interpretation: a gradebook include either the assignment
; columns or calculated columns

(define GB-0 AS-0)
(define GB-1 AS-1)
(define GB-2 AS-2)
(define GB-3 AS-3)
(define GB-4 AS-4)
(define GB-5 AS-5)
(define GB-6 AS-6)
(define GB-7 AS-7)
(define GB-8 AS-8)
(define GB-9 AS-9)
(define GB-10 AS-10)
(define GB-11 AS-11)
(define GB-12 CAL-1)
(define GB-13 CAL-2)
(define GB-14 CAL-3)
(define GB-15 CAL-4)
(define GB-16 CAL-5)
(define GB-17 CAL-6)


(define (gradebook-temp g)
  (cond
    [(assignment? g) (assignment-temp g)]
    [(calculated? g) (calculated-temp g)]))

; TODO #2: design the function valid-gradebook, which makes sure...
; - the names of all columns aren't empty
; - the number of dropped grades is always smaller
;   than the number of columns in that calculated column
; - the weights in a weighted average make sense: there is
;   one for each column, they are all positive, and they
;   add up to 100%

; valid-gradebook : Gradebook -> Boolean
; determines if the gradebook has nonempty names for all columns,
; smaller number for dropped grades than for the total columns,
; and all positive weighted averages, which add up to 100%

(check-expect (valid-gradebook GB-0) #false)
(check-expect (valid-gradebook GB-17) #true)

; (define CAL-6 (make-calculated "total" OP-3 (list CAL-1 CAL-2 CAL-5)))
(define (valid-gradebook g)
  (cond
    [(assignment? g) (not(string=? (assignment-name g) ""))]
    [(calculated? g)
     (cond
     [(number? (calculated-operation g)) (> (length (calculated-column g)) (calculated-operation g))]
     [(list? (calculated-operation g)) (= (foldr + 0 (calculated-operation g)) 100)])]))

