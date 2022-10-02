;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab2 拍|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;Celsius is a real number
;interpretation: degree of temperature measured in  Celsius
(define CELSIUS-MIN 20)
(define CELSIUS-MAX 25)
(define (celsius-temp c)
  (... c ...))
;Humidity is a real number
;humidity is the moisture hold in the air
(define HUMIDITY-MIN 30)
(define HUMIDITY-MAX 50)
(define (humidity-temp h)
  (... h...))

; good-weather?: Celsius Humidity-> Boolean
; determine whether is a good weather by checking temperature and humidity
(check-expect (good-weather? 21 40) "good weather")
(check-expect (good-weather? 19 40) "bad weather")
(check-expect (good-weather? 21 60) "bad weather")


(define (good-weather? c h)
  (if (and (< CELSIUS-MIN c)(> CELSIUS-MAX c)) (if (and (< HUMIDITY-MIN h)(> HUMIDITY-MAX h)) "good weather" "bad weather") "bad weather"))




