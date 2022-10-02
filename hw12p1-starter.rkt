;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw12p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 12, Problem 1 ==

; In this problem you will be working with the shift cipher, also known as Caesar sipher,
; one of the simplest encryption techniques (https://en.wikipedia.org/wiki/Caesar_cipher).

; The basic idea is to shift every letter in an alphabet by a constant amount (circling
; back if it goes past the end of the alphabet). For example, shifting the alphabet by 1
; would produce...

; Start:  ABC ... XYZ
; Cipher: BCD ... YZA

; The constant can also be negative, such as -3...

; Start:  ABC ... XYZ
; Cipher: XYZ ... UVW 

; To help, you've been supplied two functions to help translate between upper-case letters
; (e.g., "A" "B" "C") and the corresponding offsets from the start of the English alphabet
; (e.g.,  0   1   2)...


; alphabet : Nat[0, 25] -> 1String
; provides the letters of the alphabet given an
; English-alphabet offset (i.e., 0=A and 25=Z)

(check-expect
 (alphabet 0)
 "A")

(check-expect
 (alphabet 25)
 "Z")

(define (alphabet n)
  (string
   (integer->char (+ 65 n))))

; alphabet-offset : 1String -> Nat[0, 25]
; provides the offset in the English alphabet (i.e., A=0, Z=25)

(check-expect
 (alphabet-offset "A")
 0)

(check-expect
 (alphabet-offset "Z")
 25)

(define (alphabet-offset letter)
  (-
   (char->integer
    (string-ref letter 0))
   65))


; (In case you are curious about the magic number 65, this conversion is based on the ASCII
; character encoding: https://en.wikipedia.org/wiki/ASCII).



; TODO #1: design the function encrypt that takes a string (you can assume it is only composed of
; upper-case letters and spaces) and a shift and produces the corresponding encrypted message,
; ignoring the spaces. You have been provided some example tests for clarity.

; Hint: the modulo function can help deal with shift numbers that are less than 0 or greater than
; 25; it basically works like remainder but works better in this context for negative numbers.

; reference: from Hw10 p2
; A SpaceSeparatedString (SSS) is one of:
; - ""
; - (string-append " " SSS)
; - (string-append CString SSS)

(define SSS-EMPTY "")
(define SSS-SPACE " ")

(define (sss-temp sss)
  (...
   (cond
     [(string=? s SSS-EMPTY) ...]
     [(string=? (substring sss 0 1) SSS-SPACE)
      ... (sss-temp (substring sss 1)) ...]
     [else
      ... (substring sss 0 1) ...
      ... (sss-temp (substring sss 1)) ...])))

; encrypt : String Number -> String
; produces a encrpted message which is based on the given string and shifts the given number

(check-expect
 (encrypt "ABC" 0)
 "ABC")

(check-expect
 (encrypt "ABC" 1)
 "BCD")

(check-expect
 (encrypt "XYZ" 1)
 "YZA")

(check-expect
 (encrypt "ABC" 26)
 "ABC")

(check-expect
 (encrypt "ABC" -3)
 "XYZ")

(check-expect
 (encrypt "XYZ" -3)
 "UVW")

(check-expect
 (encrypt "ABC ABC" 1)
 "BCD BCD")

(define (encrypt lon n)
  (local [; convert: String Number -> [List-of Number]
          ; convert every character of the string to the corresponding number
          ; and produce a list of number by adding the given shift
          (define (convert l n)
            (map (λ (x) (+ x n)) (map alphabet-offset (explode l))))
          ; conversion: [List-of Number] -> [List-of Number]
          ; convert every number of given list to the corresponding
          ; number of 26 character
          (define (conversion lon)
            (cond
              [(empty? lon) '()]
              [(cons? lon)
               (cons
                (if (or (positive? (first lon)) (zero? (first lon)))
                    (remainder (first lon) 26)
                    (remainder (+ 26 (first lon)) 26))
                (conversion (rest lon)))]))
          ; every-conversion: [List-of [List-of Number]] -> String
          ; adds the space in front of every given list of characters
          ; that are represent by the given list of numbers
          (define (every-conversion lon)
            (cond
              [(empty? lon) ""]
              [(cons? lon)
               (string-append " "
                              (foldr string-append "" (map alphabet (first lon)))
                              (every-conversion (rest lon)))]))]
    (substring (every-conversion (map (λ (x) (conversion x))
                                      (map (λ (x) (convert x n)) (split-on-space lon)))) 1)))

; reference: from Hw10 p2
; location-of-first-space : SSS -> Nat
; returns the location of the character that is the
; first space in the string (or length of the string
; if there are no spaces)

(check-expect (location-of-first-space "") 0)
(check-expect (location-of-first-space " ") 0)
(check-expect (location-of-first-space "hello") 5)
(check-expect (location-of-first-space "hello world") 5)
(check-expect (location-of-first-space "hi again world") 2)

(define (location-of-first-space s)
  (cond
    [(string=? s SSS-EMPTY) 0]
    [(string=? (substring s 0 1) SSS-SPACE) 0]
    [else (add1 (location-of-first-space (substring s 1)))]))


; split-on-space : SSS -> [List-of CString]
; produces a list of words by splitting on spaces

(check-expect (split-on-space "") '())
(check-expect (split-on-space " ") '())
(check-expect (split-on-space "  ") '())
(check-expect (split-on-space "hello") (list "hello"))
(check-expect (split-on-space "hello world") (list "hello" "world"))
(check-expect (split-on-space "hi again world") (list "hi" "again" "world"))

(define (split-on-space s)
  (cond
    [(string=? s SSS-EMPTY) '()]
    [(string=? (substring s 0 1) SSS-SPACE) (split-on-space (substring s 1))]
    [else
     (local [(define FIRST-SPACE (location-of-first-space s))]
       (cons (substring s 0 FIRST-SPACE)
             (split-on-space (substring s FIRST-SPACE))))]))

; TODO #2: design the function decrypt that takes an encrypted message, and tries to decrypt it.
; It does so by considering an additional pair of inputs, presumed to be a pairing between an
; original phrase and its encrypted version. Your function must try to find the constant offset
; between this pair, and then apply it to decrypt the first input; if such an offset isn't found
; then the function should return #false. Some tests have been supplied for clarity.

; To guide your design, you have also been supplied the signature and test for a helper (find-shift);
; you must meaningfully use this function in your solution without changing its signature. This
; function should start at a shift of 0, see if encrypting using that offset produces a matching
; encrypted string: if so, return that, otherwise continue counting up until 26 (a full cycle of the
; alphabet). Don't forget accumulator and termination statements as appropriate!

; decrypt: String String String -> String
; decrypt the first given string by finding the shifts from the second and the third strings

(check-expect
 (decrypt
  "QEB NRFZH YOLTK CLU GRJMP LSBO QEB IXWV ALD"
  "DEF"
  "ABC")
 "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG")

(check-expect
 (decrypt
  "ABC"
  "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
  "QEB NRFZH YOLTK CLU GRJMP LSBO QEB IXWV ALD")
 "DEF")

(define (decrypt s1 s2 s3)
  (encrypt s1 (find-shift s3 s2)))

; find-shift : String String -> [Maybe Nat[0, 25]]
; returns the corresponding shift of the given two strings
; Termination: starts from 0 and checks if it's the correct shift
; and adds one every time until it's 26 and returns #false

(check-expect
 (find-shift
  "ABC" "DEF")
 3)

(check-expect
 (find-shift
  "ABC" "ABD")
 #false)

(define (find-shift s1 s2)
  (local [; find-shift/acc : Number -> Number
          ; returns the corresponding shifts of the given two strings starting from zero
          ; Accumulator : current shift so far 
          (define (find-shift/acc acc)
            (cond
              [(= acc 26) #false]
              [(< acc 26)
               (if (string=? s2 (encrypt s1 acc)) acc
                   (find-shift/acc (add1 acc)))]))]
    (find-shift/acc 0)))

