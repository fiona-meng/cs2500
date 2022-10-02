;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname l12p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Lab 12, Problem 1 ==

; TODO #1: recall that a natural number can be defined as follows...

; A Nat is one of:
; - 0
; - (add1 Nat)
; Interpretation: a natural number

; Design the function power, which takes two natural numbers and computes
; the first number to the power of the second number using successive
; multiplication (where ^ means "raised to the power of")...

; base^0 = 1
; base^1 = base * base^0 = base
; base^2 = base * base^1 = base * base
; base^3 = base * base^2 = base * base * base
; base^4 = base * base^3 = base * base * base * base
; ...
; base^12 = base * base^11 = base * base * base * base * base * base *
;                            base * base * base * base * base * base
; ...

; and following the template that arises from the above data definition.
; Your tests can make use of the built-in expt function if you wish (but
; of course your implementation cannot).

; power: Nat Nat-> Nat
; find the a supplied number to the supplied power

(check-expect (power 10 2) (expt 10 2))
(check-expect (power 500 9) (expt 500 9))
(check-expect (power 88 5) (expt 88 5))

(define (power n p)
  (local [; power/acc: Nat Nat -> Nat
          ; power the supplied natural number
          ; Accumulator: current power
          (define (power/acc base p)
            (if (= p 0)
                base
                (power/acc (* n base) (sub1 p))))]
    (power/acc 1 p)))


; TODO #2: design the function power-fast, which uses a faster algorithm to
; answer the same problem based on the folllowing mathematical property of
; exponents...

; if the exponent (y) is odd: base^(2 * y + 1) = base * (base^(2 * y))
; if the exponent (y) is even: base^(2 * y) = (base^y) * (base^y)

; For example...
; 2^12 = 2^6 * 2^6 = 64 * 64 = 4096
; 2^6 =  2^3 * 2^3 = 8 * 8   = 64 
; 2^3 =  2 * 2^2   = 2 * 4   = 8
; 2^2 =  2^1 * 2^1 = 2 * 2   = 4
; 2^1 =  2 * 2^0   = 2 * 1   = 2
; 2^0 =  1

; (Notice that the number of steps for 2^12 is far fewer than the 12 multiplications
; we'd have to do with the first approach, since we get to skip a bunch of intermediate
; exponents AND each time we compute an even exponent, we just have to get it's result
; once and multiply them together.)

; IMPORTANT: since you are deviating from the template of a natural number, don't
; forget your termination statement!

; power-fast: Nat Nat -> Nat
; using the fast way to find a supplied number's power
; by seperate the exponent
; Termination: when the exponent become 0, the fucntion will end

(check-expect (power-fast 2 12) (expt 2 12))
(check-expect (power-fast 2 6) (expt 2 6))

(define (power-fast n p)
  (cond
    [(= p 0) 1]
    [(even? p)
     (* (power-fast n (/ p 2)) (power-fast n (/ p 2)))]
    [else
     (* n (power-fast n (sub1 p)))]))
     





