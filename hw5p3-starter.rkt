;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw5p3-starter) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 5, Problem 3 ==

; Over a few homework assignments, we are going to build
; up to some interesting and impactful methods of text
; analysis - that is, using a computer to gain insights
; from data expressed in natural language as text.

; For now, you are going to design the major data types
; we will use throughout the rest of the project.
; So for each TODO below, follow the design recipe for
; data given the description and examples.

; (And don't worry if at this point the ideas don't make
; a lot of sense; for now, this is great practice with
; designing data using structures, itemizations, and
; lists.)



; TODO #1: design WordCount, which is used to represent
; how many times a particular word appears in a body
; (or "corpus") of text. For example, we might want to
; represent that the word "text" appeared 2 times in the
; phrase "this text has a lot of text I think!".
; (Note: your data should not include the phrase itself,
; only the word and count.)

(define-struct WordCount [word count])
; A WordCount is a (make-WordCount String Number)
; Interpretation: a supplied word and a number
; count the times when a supplied word appear in the phrase
(define WordCount-1 (make-WordCount
         "text" 2))
(define WordCount-2(make-WordCount
         "hello" 2))

(define (WordCount-temp w)
  (... (WordCount-word w) ...
       (WordCount-count w) ...))


; TODO #2: design CorpusWordCounts, which is a list of all
; the words in a corpus along with their counts. So, for
; example, in the sentence "hello world hello", the word
; "hello" appears 2 times and the word "world" appears 1
; time. You should make use of WordCount in your definition.
; (Note: your data should not include the corpus itself,
; only the words and their associated counts.)

; A CorpusWordCounts is one of:
; - '()
; - (cons WordCount CorpusWordCounts)
; Interpretation: a list of WordCount
(define CorpusWordCounts-1
  (cons WordCount-2
        (cons WordCount-1 '())))
(define (CorpusWordCounts-temp c)
  (...
   (cond
     [(empty? c) ...]
     [(cons? c) ...
      (WordCount-temp (first c))...
      (CorpusWordCounts-temp (rest c))...])))


; TODO #3: design Polarity, which is a way of describing
; a body of text as being either positive, negative, or
; neutral. For example, the phrase "What a wonderful day!"
; might be labeled as positive, while "2020 has been a rough
; year :(" might be labeled as negative.
; (Note: your data should not include the text itself,
; only the label that can take these three values.)

; A Polarity is one of:
; -"positive"
; -"negative"
; -"neutral"
; Interpretation: three different attitudes
(define pos "positive")
(define neg "negative")
(define neu "neutral")
(define (Polarity-temp p)
  (...
   (cond
     [(string=? p pos) ...]
     [(string=? p neg) ...]
     [(string=? p neu) ...])))

; TODO #4: design SentenceLabel, which is a value of -1.0, 1.0,
; or 0.0 that a reader might numerically assign a particular
; sentence. We will understand these values as corresponding to
; a Polarity value (i.e., 1.0 = positive, -1.0 = negative,
; 0.0 = neutral), but your definition just needs to represent a
; type that can only take these three numeric values.
; A score is one of:
; - -1.0
; - 1.0
; - 0.0
; Interpretation: three scores corresponding to the Polarity
(define pos-score 1.0)
(define neg-score -1.0)
(define neu-score 0.0)
(define (score-temp s)
  (...
   (cond
     [(equal=? s pos-score) ...]
     [(equal=? s neg-score) ...]
     [(equal=? s neu-score) ...])))


; TODO #5: design WordPolarityData, which is a way of representing
; polarity data that we collect about a word in a collection of
; sentences, including the word, the number of times it occurred in
; the corpus, and the sum of a polarity score (a positive or negative
; real number) representing how positive/negative the word has
; appeared. For example, we might represent that the word "happy"
; has occurred 4 times in a body of text, and its polarity sum is 1.2.
; As another example, perhaps the word "sad" has occurred 3 times in
; a body of text, and its polarity sum is -3.6.
; (Note: your data should not include the text itself, only the word
; and its associated polarity data.)
(define-struct WordPolarityData [WordCount Polarity])
; A WordPolarityData is a (make-WordPolarityData WordCount Polarity)
; Interpretation: a supplied word and a number with three different attitudes
(define WordPolarityData-1 (make-WordPolarityData (make-WordCount "happy" 4) 1.2))
(define WordPolarityData-2 (make-WordPolarityData (make-WordCount "sad" 3) -3.6))
(define (WordPolarityData-temp w)
  (... (WordCount-temp (WordPolarityData-WordCount w))
       (Polarity-temp(WordPolarityData-Polarity w)) ...))



; TODO #6: design CorpusWordPolarityData which is a list of all
; the words in a corpus along with their polarity data. So continuing
; the last example, we'd want to have a list with both "happy"
; (along with 4 and 1.2) as well as "sad" (along with 3 and -3.6).
; You should make use of WordPolarityData in your definition.
; (Note: your data should not include the text itself, only the words
; and their associated polarity data.)

; A CorpusWordPolarityData is one of:
; - '()
; - (cons WordPolarityData CorpusWordPolarityData)
; Interpretation: a list of WordPolarityData
(define CorpusWordPolarityData-1
  (cons WordPolarityData-1
        (cons WordPolarityData-2 '())))
(define (CorpusWordPolarityData-temp c)
  (...
   (cond
     [(empty? c) ...]
     [(cons? c) ...
      (WordPolarityData-temp (first c))...
      (CorpusWordPolarityData-temp (rest c))...])))

