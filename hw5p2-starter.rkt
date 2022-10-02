;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw5p2-starter) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 5, Problem 2 ==

; You are to design a reminders program, which
; allows you to keep track of all the tasks in
; your busy digital life.

; In this program, a Group is an organizational unit
; that has a title (such as "Today") and a list of tasks.
; Each Task has it's own descriptive name (such as
; "Submit Homework 5") as well as an indication of whether
; the task has been completed or not.

; When you run reminders, you supply it a group; the
; program should then show you the group's title and
; the first task (including it's description and some
; way of indicating whether it's been completed or not);
; if the task list is empty, a friendly congratulations
; might be in order ;)
;
; Pressing the "n" key on the keyboard (for "next") should
; allow you to scroll through the tasks in order - it should
; cycle such that after seeing the last task, you come back to
; the first. When viewing a task, pressing space bar toggles
; whether the task is complete or not. When the program ends,
; it should return the number of tasks in the group that are
; incomplete.

; TODO #1: Finish designing the data you will need for this program.
; It will be useful (and fun!) to have a reasonable number of
; examples.

(define-struct task [desc done?])

; A Task is a (make-task String Boolean)
; Interpretation: the reminder task and whether it is complete
(define task-1 (make-task "finish Homework5" #false))
(define task-2 (make-task "finish Lab6" #true))
(define task-3 (make-task "finish reading assignment" #false))

(define (task-temp task)
  (... (task-desc task) ...
       (task-done? task) ...))


; A ListOfTasks (LoT) is one of:
; - '()
; - (cons Task LoT)
; Interpretation: a list of tasks
(define LoT-0 '())
(define LoT-1
  (cons task-1'()))

(define LoT-2
  (cons task-2
        (cons task-1 '())))

(define LoT-3
  (cons task-3
        (cons task-2
              (cons task-1'()))))
(define (lot-temp lot)
  (...
   (cond
     [(empty? lot) ...]
     [(cons? lot) ...
      (task-temp (first lot))...
      (lot-temp (rest lot))...])))

(define-struct group [title tasks])

; A Group is a (make-group String LoT)
; Interpretation: a titled task list
(define group-1 (make-group "Today" LoT-3))
(define (group-temp g)
  (... (group-title g) ...
       (lop-temp g) ...)) 


; TODO #2: Finish designing the World program reminders.
; You are welcome to be creative as to how the program
; visualizes the group, the task, and completion status.
; Hint: for each function we started, follow the template
; closely to determine any further helper(s).


; reminders : Group -> Nat
; Visualizes an interactive reminders list
; and returns the number of remaining incomplete
; tasks at exit

(define (reminders initial-group)
  (num-not-complete
   (big-bang initial-group
     [to-draw draw-group]
     [on-key key-group])))


; num-not-complete : Group -> Nat
; counts the number of incomplete tasks
; in the group
(check-expect (num-not-complete group-1) 2)

(define (num-not-complete group)
  (count (group-tasks group))) 
  
; count: LoT->Number
; find the total number of task has not been finished
(check-expect (count LoT-1)1)
(check-expect (count LoT-0)0)

(define (count lot)
  (cond
    [(empty? lot) 0]
    [(cons? lot) 
     (+(finish? (first lot))
       (count (rest lot)))]))

; finish?: Task->Number
; determine whether the task has been completed
; if it has been completed return 0,
; otherwise return 1
(check-expect (finish? task-1) 1)
(check-expect (finish? task-2) 0)
(define (finish? task)
  (if (task-done? task) 0 1))



; draw-group : Group -> Image
; visualizes the reminder group
(check-expect (draw-group group-1)
              (place-image (above (text "Today" 20 "purple")
                                  (text "finish reading assignment not finish" 15 "black"))
                           150 150 Background))
                            
(define Background (square 300 "solid" "white"))
(define (draw-group group)
  (place-image (above
                (text (group-title group) 20 "purple")
                (draw-LoT (group-tasks group)))
               150 150 Background))
                                  
; draw-LoT: LoT->Image
; visualizes the reminder LoT
(check-expect (draw-LoT LoT-1)
              (text "finish Homework5 not finish" 15 "black"))
(check-expect (draw-LoT LoT-0)
              (text " " 15 "black"))
(define (draw-LoT lot)
  (cond
    [(empty? lot)(text " " 15 "black") ]
    [(cons? lot)
     (draw-task (first lot))]))


; draw-task: Task->Image
; visualize the reminder task
(check-expect (draw-task task-1)
              (text "finish Homework5 not finish" 15 "black"))
(check-expect (draw-task task-2)
              (text "finish Lab6 finish" 15 "black"))
(define (draw-task task)
  (text
   (string-append (task-desc task) " "
                  (if (task-done? task) "finish" "not finish"))
   15
   "black"))


; key-group : Group KeyEvent -> Group
; when "n" is pressed, rotate's the group's
; task list (first goes on the end);
; when " " is pressed, flips the completion
; status of the current task
(check-expect (key-group group-1 " ")
              (place-image
               (text (number->string 2) 20 "red")
               150 150 Background))
              

(define (key-group group ke)
  (cond
    [(key=? ke "n")
     (advance-draw (group-tasks group))]
    [(key=? ke " ")
     (place-image
      (text (number->string(num-not-complete group)) 20 "red")
      150 150 Background)]))

; advance-draw: LoT->LoT
; Advance draw can move to the next lot
(check-expect (advance-draw LoT-3)
              (cons (make-task "finish Lab6" #true)
                    (cons (make-task "finish Homework5" #false)
                          (cons
                           (make-task "finish reading assignment" #false)
                           '()))))
              
(define (advance-draw lot)
  (cond
    [(empty? (first lot)) '()]
    [(cons? lot) 
     (append (rest lot)(cons(first lot) '()))]))







     




  
