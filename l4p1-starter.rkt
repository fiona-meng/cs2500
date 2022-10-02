;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname l4p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Lab 4, Problem 1 ==

; So maybe you've seen and/or played these games that
; follow a simple pattern: your character runs and
; jumps to gather coins/candy/etc (which equal points).
; So in this lab, you will build up to such a game
; while practicing designing union & self-referential
; data, as well as functions & programs that use it.

; We know this looks like a LONG program, but we've
; broken it up into small bits, so do your best and
; at least try every part - you can do this!!!


(define-struct level [should-jump? next])

; A JumpLevel is one of:
; - "done"
; - (make-level Boolean JumpLevel)
; Interpretation: design of the correct actions
; in a jump game, either done or...
; - should-jump? is what the player should do
; - next is the next level of the game
(define JumpLevel-0 #true)
(define JumpLevel-1 (make-level #true JumpLevel-0))
(define JumpLevel-2 (make-level #true JumpLevel-1))
(define (JumpLevel-temp j)
  (...
   (cond
     [(boolean? j) ...]
     [(level? j) ...
      (JumpLevel-should-jump? j) ...
      (JumpLevel-temp (JumpLevel-next j) ...)])))

; TODO #1: complete the data design recipe for the
; above data definition.

; TODO #2: design the function level-done?
; that accepts a JumpLevel and returns
; #true if the level done (and #false otherwise).
; (This is useful in making a game-end screen!)

; level-done?: JumpLevel -> Boolean
; to decide whether the game end
(check-expect (level-done? JumpLevel-1) #true)
(define (level-done? j)
  (cond
     [(boolean? j) #true]
     [(level? j)
      (level-done? (level-next j))]))
  

; TODO #3: finish designing the function
; add-point that accepts a JumpLevel and
; a Boolean value representing whether the
; player opted to jump in the current level
; and returns 1 if the player should get a
; point for their (correct) action, 0 otherwise.
; So if the level is done, no points are awarded;
; otherwise, if the player action matches what
; they *should* have done, they get a point.
; (This is useful in updating the player's score!)



; add-point : JumpLevel Boolean -> {0, 1}
; given a level and whether or not the player jumped,
; returns 1 if a point should be awarded, 0 otherwise
(check-expect (add-point JumpLevel-1) 1)
(check-expect (add-point JumpLevel-2) 1)
(define (add-point j)
  (cond
    [(equal? (level-should-jump? j) (level? j)) 1]
    [else 0]))
     

; TODO #4: Design the function progress-level
; that returns the next level after a supplied
; level, where "done" is always supplied after
; "done".
; (This is useful for moving the player to the
; next level after they jump/run!)

; progress-level: JumpLevel -> JumpLevel
; allow user to go next jump level
(check-expect (progress-level JumpLevel-0) "done")
(define (progress-level j)
   (cond
     [(level-done? j) "done" ]
     [(level? j)
      (level-next j)]))

; TODO #5: finish designing the function instructions,
; which produces the sequence of instructions to a
; player to get a perfect score in the game.
; An example set of instructions might be "run-jump-done".
; (This isn't useful in making the game, but it's
; good practice with this type of data!)

; instructions : JumpLevel -> String
; produces a sequence of instructions to perfectly
; play a jump game
(define r "run")
(define u "jump")
(define d "done")
(define (instructions j)
  (cond
    [(string=? j r) "run"]
    [(string=? j u) "jump"]
    [(string=? j d) "done"]))
    
(define-struct game [score level])

; A Game is a (make-game Nat JumpLevel)
; Interpretation: status of game play
; - score is the current score
; - level is the current level
(define Game-1 (make-game 10 15))
(define Game-2 (make-game 11 16))
(define (game-temp g)
  (... (game-score) ...
       (game-level) ...))

; TODO #6: complete the data design recipe for the
; above data definition (be sure to create examples
; that represent how a player might progress through
; a full game, since these will be useful later!).


; TODO #7: finish designing the draw-game function,
; which serves as the to-draw for big-bang.
; You should follow the template for Game, meaning
; that you'll need a helper to visualize the level
; (and then join that together somehow with the
; score you extract from the supplied game).
; You are welcome to be creative with how your game
; looks :)


; draw-game : Game -> Image
; visualizes the game state
(define (draw-game g)
  (make-game (game-score g)
          (draw-level (game-level g))))
(define Background (square 500 "solid" "white"))
; draw-level : JumpLevel -> Image
; visualizes the part of the screen
; that depends on the level
(check-expect (draw-level JumpLevel-0 )
              (place-image (text (number->string(add-point JumpLevel-0 ))
                                 24 "black")))
(define (draw-level j)
  (place-image (text (number->string(add-point j)) 24 "black")))


; TODO #8: finish designing the key-game
; function, which serves as the on-key handler
; for big-bang. The idea here is that the player
; signifies they want to jump with the "j" key
; and run with the "r" key; your function
; should then produce the next state of the
; game, possibly updating the score and/or
; level.
;
; Hint: for a good design, let this function
; figure out which key was pressed and, if it
; was "j" or "r", call a helper with a Boolean
; indicating whether the player jumped; that
; function can then use the Game template to
; make a new game, while taking advantage of
; the add-point and progress-level helpers you
; designed in the beginning :)


; key-game : Game KeyEvent -> Game
; changes the game based upon the key pressed
(define (key-game g ke)
  (cond
    [(key=? ke "r") (instructions r)]
    [(key=? ke "j") (instructions u)]
    [(key=? ke "d") (instructions d)]))
    

; progress-game : Game Boolean -> Game
; produces the effect of jumping (or not) in the
; current game state
(define (progress-game a b) ...)
  
  


; TODO #9: finish designing the done-game?
; function, which serves as the stop-when
; handler in big-bang. The idea is just to
; return #true if the current level is "done".
; Your function should follow the Game
; template, which means calling a helper based
; on the JumpLevel template, which ... cough ...
; you may have already written above :)


; done-game? : Game -> Boolean
; is the game done yet?
(check-expect (done-game? JumpLevel-0) #true)
(define (done-game? j)
  (level-done? j))


; TODO #10: uncomment the function below and
; replace the ???'s with real BSL. The goal
; is that we initialize big-bang with a game
; that has zero score and the supplied level,
; and that when the game is done we are only
; returning the final score

; play : JumpLevel -> Nat
; plays the supplied levels and returns
; the final player score

(define (play jl)
   (big-bang jl
     [to-draw draw-game]
     [on-key key-game]
     [stop-when done-game? draw-game]))


; OMG - amazing work, well done!!!!
; Now play YOUR game :)
