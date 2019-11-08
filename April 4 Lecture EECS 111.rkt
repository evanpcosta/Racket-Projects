;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |April 4 Lecture EECS 111|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;; Lecture 2

(define INCHES/FOOT 12)

#|
The Simplest Design Recipe
1. Signature/purpose/header
2. Examples
3. Body
4. Tests
|#

; https://github.com/tov/pull-tool.git

#|
Design a function that computes the area of a
triangle from its base and height.
|#


; area-of-circle : Number -> Number
; Finds the area of a circle.
;
; Examples:
;  - When r = 1, area is π
;  - When r = 4, area is 16π
(define (area-of-circle radius)
  (* pi (sqr radius)))

(check-within (area-of-circle 1) pi        0.000001)
(check-within (area-of-circle 4) (* 16 pi) 0.000001)


; volume-of-cylinder : Number Number -> Number
; Finds the volume of a cylinder.

(define (area-of-circle radius)
  (define height-of-cylider h)
  (* h (* pi (sqr radius)))

(check-within (area-of-circle 1) (height-of-cylider 12) (* 12 pi)   0.000001)
(check-within (area-of-circle 4) (height-of-cylider 12) (* 192 pi) 0.000001)
  
; friendly-greeting : String -> String
; Produces a friendly greeting.
;
; Examples:
;  - (friendly-greeting "EECS 111") => "Hello, EECS 111!"
;  - (friendly-greeting "Tristan") => "Hello, Tristan!"
(define (friendly-greeting who)
  (string-append "Hello, " who "!"))

(check-expect (friendly-greeting "EECS 111")  "Hello, EECS 111!")
(check-expect (friendly-greeting "Tristan")  "Hello, Tristan!")



; milk-or-oj? : String -> Boolean
; Determines whether `drink` is one of "milk" or "oj".
;
; Examples:
;  - (milk-or-oj? "milk") should be #true
;  - (milk-or-oj? "water") should be #false
(define (milk-or-oj? drink)
  (or (string=? drink "milk")
      (string=? drink "oj")))

(check-expect (milk-or-oj? "milk") #true)
(check-expect (milk-or-oj? "oj") #true)
(check-expect (milk-or-oj? "water") #false)

; feet->inches : Number -> Number
; Converts a length from feet to inches.
;
; Examples:
;  - 1 foot => 12 inches
;  - 5 foot => 60 inches
(define (feet->inches ft)
  (* INCHES/FOOT ft))

(check-expect (feet->inches 1) 12)
(check-expect (feet->inches 5) 60)

(require 2htdp/image)
;prohibit : Image -> Image 
;Adds a red, slashed circle over the given image.
;
; Examples:
; -(Prohibit [cigarette]) => [no smoking sign]
; -(prohibit [letter P]) => [no parking sign]
(define (prohibit img)
  overlay
   (slash-circle (max (image-width img) (image-height img)))
  img))