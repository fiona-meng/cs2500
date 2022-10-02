;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname l7p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Lab 7, Problem 1 ==

; For each TODO below, refer to the following data definitions,
; and use pre-defined list abstraction(s) when appropriate.

; Note #1: try to choose appropriate abstraction(s), finding a
; good match for an effective function design!

; Note #2: just because we now have cool abstractions doesn't mean
; you should forget about the design recipe and following templates
; (which particularly come up for abstraction helpers)!

; Note #3: feel free to create additional examples if you'd like
; (you'll probably need some for later tests anyway!)


; A Genre is one of
; - "Pop"
; - "Classical"
; - "Country"
; - "Rock"
; Interpretation: a song genre

(define GENRE-POP "Pop")
(define GENRE-CLASSICAL "Classical")
(define GENRE-COUNTRY "Country")
(define GENRE-ROCK "Rock")

(define (genre-temp g)
  (...
   (cond
     [(string=? g GENRE-POP) ...]
     [(string=? g GENRE-CLASSICAL) ...]
     [(string=? g GENRE-COUNTRY) ...]
     [(string=? g GENRE-ROCK) ...])))


(define-struct song [name artist duration genre fav?])

; A Song is a (make-song String String Nat Genre Boolean)
; Interpretation: a song
; - name is the title of the song
; - artist is the song's artist
; - duration is the length in seconds
; - genre is the song's genre
; - fav? is this a liked song?

(define SONG-1
  (make-song
   "Redesigning Women"
   "The Highwomen"
   174 GENRE-COUNTRY
   #true))

(define SONG-2
  (make-song
   "Your Song"
   "Elton John"
   241 GENRE-POP
   #true))

(define SONG-3
  (make-song
   "All Along the Watchtower"
   "Jimi Hendrix"
   241 GENRE-ROCK
   #false))

(define SONG-4
  (make-song
   "Nessun Dorma"
   "Luciano Pavarotti"
   184 GENRE-CLASSICAL
   #false))

(define (song-temp song)
  (... (song-name song) ...
       (song-artist song) ...
       (song-duration song) ...
       (genre-temp (song-genre song)) ...
       (song-fav? song) ...))

(define-struct pl [name songs])

; A Playlist is a (make-pl String [List-of Song])
; Interpretation: a sequence of songs

(define PL-0
  (make-pl "Quiet :)" '()))

(define PL-1
  (make-pl
   "Coding Beats"
   (list SONG-1 SONG-2
         SONG-3 SONG-4)))

(define (pl-temp pl)
  (... (pl-name pl) ...
       (los-temp (pl-songs pl)) ...))



; TODO #1: design the function any-pop? that determines
; if a playlist has any pop songs

; any-pop?: Playlist->Boolean
; are there any pop songs in the playlist?
(check-expect (any-pop? PL-0) #false)
(check-expect (any-pop? PL-1) #true)
(define (any-pop? pl)
  (ormap pop? (pl-songs pl)))

; pop?: Song->Boolean
; any pop song?
(check-expect (pop? SONG-1) #false)
(check-expect (pop? SONG-2) #true)
(check-expect (pop? SONG-3) #false)
(check-expect (pop? SONG-4) #false)
(define (pop? song)
  (cond
    [(string=? (song-genre song) GENRE-POP) #true]
    [(string=? (song-genre song) GENRE-CLASSICAL) #false]
    [(string=? (song-genre song) GENRE-COUNTRY) #false]
    [(string=? (song-genre song) GENRE-ROCK) #false]))

; TODO #2: design the function all-short? that determines
; if a playlist contains only songs shorter than 5 minutes

; all-short?: Playlist->Boolean
; determine whether the playlist only contains songs,
; which less than 5 minutes
(check-expect (all-short? PL-0) #true)
(check-expect (all-short? PL-1) #false)
(define (all-short? pl)
  (andmap short? (pl-songs pl)))

; short?: Song->Boolean
; determine whether the song is less than 5 mintues
(check-expect (short? SONG-1) #false)
(check-expect (short? SONG-2) #false)
(check-expect (short? SONG-3) #false)
(check-expect (short? SONG-4) #false)
(define (short? song)
  (< (song-duration song) 5))


; TODO #3: design the function total-duration that returns
; the total length of a playlist in seconds

; total-duration: Playlist->Number
; find the total duration of the playlist
(check-expect (total-duration PL-0) 0)
(check-expect (total-duration PL-1) 840)
(define (total-duration pl)
  (total-time (pl-songs pl)))
; total-time: [List-of Song]->Number
; find a the total duration of a list of song
(check-expect (total-time
               (list SONG-1 SONG-2 SONG-3 SONG-4)) 840)
(check-expect (total-time (list))0)
(define (total-time los)
  (foldr + 0 (list-of-duration los)))

; list-of-duration: [List-of Song]->[List-of Song]
; find the list of duration of the song
(check-expect (list-of-duration
               (list SONG-1 SONG-2
                     SONG-3 SONG-4))
              (list 174 241 241 184))
(check-expect (list-of-duration
               (list SONG-1 SONG-2))
              (list 174 241 ))
(define (list-of-duration los)
  (map find-duration los))

; find-duration: Song->Number
; find the duration of the song
(check-expect (find-duration SONG-1) 174)
(check-expect (find-duration SONG-2) 241)
(define (find-duration song)
  (song-duration song))
  

; TODO #4: design the function all-names that
; produces a list of all the names of all the
; songs on a playlist
; all-names: Playlist->Playlist
; produce song name in the playlist
(check-expect (all-names PL-0) (make-pl "Quiet :)" '()))
(check-expect (all-names PL-1) (make-pl
                                "Coding Beats"
                                (list "Redesigning Women"
                                      "Your Song"
                                      "All Along the Watchtower"
                                      "Nessun Dorma")))
(define (all-names pl)
  (make-pl (pl-name pl) 
           (map find-name (pl-songs pl))))
; find-name: Song->String
; find the name of the song
(check-expect (find-name SONG-1) "Redesigning Women")
(check-expect (find-name SONG-2) "Your Song")
(check-expect (find-name SONG-3) "All Along the Watchtower")
(check-expect (find-name SONG-4) "Nessun Dorma")
(define (find-name song)
  (song-name song))
  

; TODO #5: design the function only-faves that when
; supplied a playlist returns a new playlist (with the
; name "Faves") that only contains the liked songs.

; only-faves: Playlist->Playlist
; find all the liked song with playlist name "Faves"
(check-expect (only-faves PL-0) (make-pl "Faves" '()))
(check-expect (only-faves PL-1)
              (make-pl
               "Faves"
               (list SONG-1 SONG-2)))
(define (only-faves pl)
  (make-pl "Faves"
           (filter like? (pl-songs pl))))
; like?: Song->Boolean
; determine whether it is liked song
(check-expect (like? SONG-1) #true)
(check-expect (like? SONG-2) #true)
(check-expect (like? SONG-3) #false)
(check-expect (like? SONG-4) #false)
(define (like? song)
  (song-fav? song))

   

; TODO #6: design the World program jukebox that shows the contents
; of a playlist. Start by showing all the songs in the playlist
; (along with their duration in seconds), but when the user
; presses the "f" key the program should swap back-and-forth
; between only showing the favorites vs all the songs. Also,
; start by showing just the titles and duration; but if a person
; presses the "a" swap back-and-forth to showing the artist names
; instead. When the program ends, return the total duration of
; the supplied playlist.

; Along the way you might find that you are writing functions
; that are quite similar - these are great opportunities for
; practicing abstraction!!

; You are welcome to use other functions you designed in this lab.
; Additionally, you should apply list abstractions where appropriate.

; And since you'll need to represent not only a playlist supplied
; to jukebox, but also whether you are showing favorite songs
; or not, as well as showing name/duration vs artist, use the
; supplied additional data definition for JS.


(define-struct js [pl only-f? show-a?])

; A JukeboxState (JS) is a (make-js Playlist Boolean Boolean)
; Interpretation: the jukebox playlist as well as...
; - only-f? is showing only favorites?
; - show-a? is showing artists?

(define JS-1-F-F
  (make-js PL-1 #false #false))

(define JS-1-T-F
  (make-js PL-1 #true #false))

(define JS-1-F-T
  (make-js PL-1 #false #true))

(define JS-1-T-T
  (make-js PL-1 #true #true))

(define (js-temp js)
  (... (pl-temp (js-pl js)) ...
       (js-only-f? js) ...
       (js-show-a? js) ...))


; jukebox : Playlist -> Nat
; interactively visualizes a playlist
; and returns the total playlist duration

;(define (jukebox js)
;  (big-bang (total-duration (js-pl js))
;    [to-draw draw-playlist]
;    [on-key key-playlist]))

(define (change js ke)
  (cond
    [(string=? ke "a") (make-js (js-pl js) (js-only-f? js) (not(js-show-a? js)))]
    [(string=? ke "f") (make-js (js-pl js) (not (js-only-f? js))(js-show-a? js))]))
; key-playlist: JukeboxState KeyEvent->JukeboxState
; if a key matches "a", show artists
; if a key matches "f", show favorites?
; otherwise show playlist

; name-f-js: JukeboxState->Image
; visualizes the title of artists
(check-expect (name-f-js JS-1-T-F)
              (text "Coding Beats Favor" 20 "red"))
(check-expect (name-f-js JS-1-F-F)
              (text "Coding Beats" 20 "red"))
              
(define (name-f-js js)
  (if (js-only-f? js)
      (text (string-append (pl-name (js-pl js)) " Favor") 20 "red")
      (text (pl-name (js-pl js)) 20 "red")))
; name-a-js: JukeboxState->Image
; visualizes the title of artists
(check-expect (name-a-js JS-1-F-T)
              (text "Coding Beats Artists" 20 "red"))
(check-expect (name-a-js JS-1-F-F)
              (text "Coding Beats" 20 "red"))
              
(define (name-a-js js)
  (if (js-show-a? js)
      (text (string-append (pl-name (js-pl js)) " Artists") 20 "red")
      (text (pl-name (js-pl js)) 20 "red")))

; jukebox-draw: JukeboxState-Image
; visualize the songs by the user perference
(check-expect (jukebox-draw JS-1-F-F)
              (above (text "Redesigning Women (174s)" 10 "black")
                     (above(text "Your Song (241s)"10"black")
                           (above (text "All Along the Watchtower (241s)"10"black")
                                  (above (text "Nessun Dorma (184s)" 10"black")
                                         (text " " 10 "black"))))))
(check-expect (jukebox-draw JS-1-T-F)
              (above (text "Redesigning Women (174s)" 10 "black")
                     (above(text "Your Song (241s)"10"black")
                           (text " " 10 "black"))))

(check-expect (jukebox-draw JS-1-F-T)
              (above (text "The Highwomen"10 "black")
                     (above(text "Elton John"10 "black")
                           (above (text "Jimi Hendrix" 10 "black")
                                  (above (text "Luciano Pavarotti"10 "black")
                                         (text " " 10 "black"))))))
(check-expect (jukebox-draw JS-1-T-T)
              (above (text "The Highwomen"10 "black")
                     (above(text "Elton John"10 "black")
                           (text " " 10 "black"))))
              
(define (jukebox-draw js)
  (cond
    [(and (boolean=? (js-only-f? js) #false) (boolean=? (js-show-a? js) #false))
     (display (pl-songs (js-pl js)))]
    [(and (boolean=? (js-only-f? js) #true) (boolean=? (js-show-a? js) #false))
     (display (filter like? (pl-songs(js-pl js))))]
    [(and (boolean=? (js-only-f? js) #false) (boolean=? (js-show-a? js) #true))
     (display-artist (pl-songs (js-pl js)))]
    [(and (boolean=? (js-only-f? js) #true) (boolean=? (js-show-a? js) #true))
     (display-artist (filter like? (pl-songs(js-pl js))))]))

; display-artist: [List-of Song]->Image
; visualize the list of artist
(check-expect (display-artist (list SONG-1 SONG-2))
              (above (text "The Highwomen"10 "black")
                     (above(text "Elton John"10 "black")
                           (text " " 10 "black"))))
(check-expect (display-artist (list))(text " " 10 "black"))
(define (display-artist los)
  (foldr above (text " " 10 "black") (list-of-artist los)))

; list-of-artist: [List-of Song]->[List-of Image]
; apply function of artist to each element in the list of song
(check-expect (list-of-artist(list SONG-1))
              (list (text "The Highwomen"10 "black")))
(check-expect (list-of-artist
               (list SONG-1 SONG-2))
              (list (text "The Highwomen"10 "black")
                    (text "Elton John"10 "black")))
                    
(define (list-of-artist los)
  (map find-artist los))

; find-artist: Song->Image
; visualize artist of the song,
(check-expect (find-artist SONG-1)
              (text "The Highwomen"10 "black"))
(check-expect (find-artist SONG-2)
              (text "Elton John"10 "black"))
(define (find-artist song)
  (text (song-artist song)10 "black"))
    

(define background (square 200 "solid" "white"))
; draw-playlist: Playlist->Image
; visualize playlist
(check-expect (draw-playlist (make-pl
                              "Coding Beats"
                              (list SONG-1 SONG-2)))
              (place-image
               (above (text "Coding Beats" 20 "purple")
                      (above (text(string-append
                                   "Redesigning Women"
                                   " ("(number->string 174)"s)")
                                  10 "black")
                             (above(text(string-append
                                         "Your Song"
                                         " ("(number->string 241)"s)")
                                        10
                                        "black")(text " " 10 "black"))))
               100 100 background))
(define (draw-playlist pl)
  (place-image
   (above (text "Coding Beats" 20 "purple")
          (display (pl-songs pl)))
   100 100 background))
; display: [List-of Song]->Image
; visualize the list of song
(check-expect (display (list SONG-1 SONG-2))
              (above (text(string-append
                           "Redesigning Women"
                           " ("(number->string 174)"s)")
                          10 "black")
                     (above(text(string-append
                                 "Your Song"
                                 " ("(number->string 241)"s)")
                                10
                                "black")(text " " 10 "black"))))
(check-expect (display (list))(text " " 10 "black"))
              
(define (display los)
  (foldr above (text " " 10 "black") (list-of-find-content los)))

; list-of-find-content: [List-of Song]->[List-of Image]
; apply function of find-content to each element in the list of song
(check-expect (list-of-find-content(list SONG-1))
              (list(text(string-append
                         "Redesigning Women"
                         " ("(number->string 174)"s)")
                        10 "black")))
(check-expect (list-of-find-content
               (list SONG-1 SONG-2))
              (list(text(string-append
                         "Redesigning Women"
                         " ("(number->string 174)"s)")
                        10 "black")
                   (text(string-append
                         "Your Song"
                         " ("(number->string 241)"s)")
                        10
                        "black")))               
(define (list-of-find-content los)
  (map find-content los))

; find-content: Song->Image
; visualize name and duration of the song,
(check-expect (find-content SONG-1)
              (text
               (string-append
                "Redesigning Women"
                " ("(number->string 174)"s)")
               10
               "black"))
(check-expect (find-content SONG-2)
              (text
               (string-append
                "Your Song"
                " ("(number->string 241)"s)")
               10
               "black"))
              
(define (find-content song)
  (text
   (string-append
    (song-name song)
    " ("(number->string(song-duration song))"s)")
   10
   "black"))



 



