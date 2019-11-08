;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lecture2_April4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(* 5 6)
;(* 3 (+ 2 9))
(define inches/foot 12)

#| This is how to write a longer comment

Simplest Design Recipe
1. Signature/Purpose/header
2. Examples
3. Body
4. Tests

|#
; milk-or-oj? : String -> Boolean
; Determines whether 'drink' is one of "milk" or "oj"
;
; Examples:
; -(milk-or-oj? "milk") should be #true
; -(milk-or-oj? "water") should be #false

(define (milk-or-oj? drink)
  (or (string=? drink "milk")
      (string=? drink "oj")))

;first thing you write is the signature
; Feet to inches : Number ----> Number
;converts a length from feet to inches.
; Examples:
; - 1 foot => 12 inches
; - 5 foot => 60 inches



(define (feet->inches ft) ...) (* inches/foot ft)

(check-expect (feet->inches 1) 12)
(check-expect (feet-> inches 5) 60)




