;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname l10p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Lab 10, Problem 1 ==

; Consider the following data definitions and examples...


(define-struct file [name size])

; A File is a (make-file String Nat)
; - name is the name of the file (including extension)
; - size is the size of the file in bytes

(define FILE-CV (make-file "cv.pdf" 466000))
(define FILE-HELLO (make-file "hello.rkt" 888))
(define FILE-PIC (make-file "pic.jpg" 968000))
(define FILE-SCHED (make-file "schedule.pdf" 288000))
(define FILE-P1 (make-file "p1.sql" 348))
(define FILE-P2 (make-file "p2.sql" 265))

(define (file-temp file)
  (... (file-name file) ...
       (file-size file) ...))

(define-struct dir [name dirs files])

; A Directory is a (make-dir String [List-of Directory] [List-of File])
; - name is the name of the directory
; - dirs is the list of sub-directories in this directory
; - files is the list of files in this directory
;   (not including the ones in sub-directories)
 
(define DIR-EMPTY (make-dir "nada" '() '()))

(define DIR-PERSONAL (make-dir "personal"
                               (list DIR-EMPTY)
                               (list FILE-CV FILE-PIC)))

(define DIR-CS2500 (make-dir "fundies" '() (list FILE-HELLO)))
(define DIR-CS3200 (make-dir "db" '() (list FILE-P1 FILE-P2)))

(define DIR-SCHOOL (make-dir "school"
                             (list DIR-CS2500 DIR-CS3200)
                             (list FILE-SCHED)))

(define DIR-ALL (make-dir "stuff" (list DIR-PERSONAL DIR-SCHOOL) '()))

(define (Directory-temp dir)
  (... (dir-name dir) ...
       ... (lod-temp (dir-dirs dir)) ...
       ... (lof-temp (dir-files dir)) ...))

(define (lod-temp lod)
  (...
   (cond
     [(empty? lod) ...]
     [(cons? lod) ...
      (Directory-temp (first lod))
      (lod-temp(rest lod)) ...])))

(define (lof-temp lof)
  (...
   (cond
     [(empty? lof) ...]
     [(cons? lof) ...
      (file-temp (first lof))
      (lof-temp(rest lof)) ...])))

; TODO #1: write the templates for File, Directory, [List-of Directory],
; and [List-of File].



; TODO #2: design the function total-files that takes a Directory
; and produces the number of files in it, however deeply they might
; be nested inside subdirectories. For example, there are 6 files
; in the supplied DIR-ALL example.

; total-files: Directory -> Nat
; returns the number of files for a given directory

(check-expect (total-files DIR-EMPTY) 0)
(check-expect (total-files DIR-CS2500) 1)
(check-expect (total-files DIR-ALL) 6)

(define (total-files dir)
  (local[; lod-num: [List-of Directory]-> Nat
         ; returns the number of files given a list od directory
         (define (lod-num lod)
           (cond
             [(empty? lod)0]
             [(cons? lod)
              (+
               (total-files(first lod))
               (lod-num (rest lod)))]))
         ; lof-num: [List-of files] -> Nat
         ; returns the total number of files given a list of files
         (define (lof-num lof)
           (cond
             [(empty? lof) 0]
             [(cons? lof)
              (length lof)]))]
    (+
     (lod-num (dir-dirs dir))
     (lof-num (dir-files dir)))))


; TODO #3: design the function file-found? that accepts a Directory and
; a string and determines if a file with that name exists in the
; directory or any of its subdirectories. For example, "hello.rkt" is a
; file that can be found in DIR-ALL.

; file-found?: Directory String -> Boolean
; determine whether a file name exists in the list of directory or
; a list of files

(check-expect (file-found? DIR-EMPTY "schedule.pdf") #false)
(check-expect (file-found? DIR-PERSONAL "cv.pdf") #true)
(check-expect (file-found? DIR-ALL "hello.rkt") #true)

(define (file-found? dir s)
  (local[; in-lod?: [List-of Directory]-> Boolean
         ; determine whether a given file name
         ; exist in the list of directory
         (define (in-lod? lod)
           (cond
             [(empty? lod) #false]
             [(cons? lod)
              (or
               (file-found? (first lod) s)
               (in-lod?(rest lod)))]))
         ; in-lof?: [List-of files] -> Boolean
         ; determine whether a given file name
         ; exist in the list of files
         (define (in-lof? lof)
           (cond
             [(empty? lof) #false]
             [(cons? lof)
              (or
               (string=? (file-name (first lof)) s)
               (in-lof? (rest lof)))]))]
    (or
     (in-lod? (dir-dirs dir))
     (in-lof? (dir-files dir)))))
               
; TODO #4: design the function rename-files that accepts a Directory
; and two Strings (src and dest), which produces a Directory with all
; the same subdirectories, but with all files named src renamed to
; dest. An example test has been supplied for clarity.

; rename-files: Directory String String -> Directory
; produce a new directory with the new file name,
; if the file exist

(check-expect
 (rename-files DIR-ALL  "pic.jpg" "pic.jpeg")
 (make-dir "stuff"
           (list
            (make-dir "personal"
                      (list DIR-EMPTY)
                      (list
                       FILE-CV
                       (make-file "pic.jpeg" 968000)))
            DIR-SCHOOL)
           '()))

(check-expect
 (rename-files DIR-EMPTY  "pic.jpg" "pic.jpeg")
 (make-dir "nada" '() '()))

(define (rename-files dir s1 s2)
  (local[; lod-name: [List-of Directory]-> [List-of Directory]
         ; change the file name if the file exists
         (define (lod-name lod)
           (cond
             [(empty? lod)'()]
             [(cons? lod)
              (cons
               (rename-files(first lod) s1 s2)
               (lod-name (rest lod)))]))
         ; lof-name: [List-of files] -> [List-of files]
         ; change the file name if the file exists given a list of files
         (define (lof-name lof)
           (cond
             [(empty? lof) '()]
             [(cons? lof)
              (cons
               (if (string=? (file-name (first lof)) s1)
                   (make-file s2 (file-size(first lof)))
                   (first lof))
               (lof-name(rest lof)))]))]
    (make-dir (dir-name dir)
              (lod-name (dir-dirs dir))
              (lof-name (dir-files dir)))))

; TODO #5: design the function big-files that accepts a Directory and a
; number of bytes and returns a list of file names in the directory with
; at least the supplied size, sorted alphabetically. An example test has
; been supplied for clarity.

; big-files: Directory Number -> [List-of String]
; return a list of file name which have the bigger bytes
; compare to the supplied bytes

(check-expect (big-files DIR-ALL 1000)
              (list "cv.pdf" "pic.jpg" "schedule.pdf"))
(check-expect (big-files DIR-ALL 10000000)'())

(define (big-files dir n)
  (local[; bytes-in-lod?: [List-of Directory]-> [List-of String]
         ; return a list of file name with the bigger bytes given
         ; a list of directory
         (define (bytes-in-lod? lod)
           (cond
             [(empty? lod) '()]
             [(cons? lod)
              (append
               (big-files (first lod) n)
               (bytes-in-lod?(rest lod)))]))
         ; bytes-in-lof?: [List-of Files]-> [List-of String]
         ; return a list of file name with the bigger bytes
         (define (bytes-in-lof? lof)
           (cond
             [(empty? lof) '()]
             [(cons? lof)
              (cons
              (if
               (> (file-size (first lof)) n)
               (file-name (first lof))
               '())
               (bytes-in-lof? (rest lof)))]))]
    (cons
     (bytes-in-lod? (dir-dirs dir))
     (bytes-in-lof? (dir-files dir)))))


