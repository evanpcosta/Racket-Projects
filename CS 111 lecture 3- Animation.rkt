;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |CS 111 lecture 3- Animation|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; CS 111 Lecture 3: Animation

(require 2htdp/image)

; flag-of-france : Natural Natural -> Image
; Creates an image of the French flag.
;
; Examples:
;  - (flag-of-france 30 20) => 30-by-20 French flag
;  - (flag-of-france 60 40) => 60-by-40 French flag
(define (flag-of-france width height)
  (tricolor width height "blue" "white" "red"))

(check-expect (flag-of-france 30 20)
              (beside
               (rectangle 10 20 "solid" "blue")
               (rectangle 10 20 "solid" "white")
               (rectangle 10 20 "solid" "red")))

; tricolor : Natural Natural Color Color Color -> Image
; Creates an image of a tricolor flag.
;
; Examples:
;  - (tricolor 30 20 "blue" "white" "red") => French flag
;  - (tricolor 80 40 "green" "white" "orange") => Irish flag
(define (tricolor width height left middle right)
  (beside
   (rectangle (/ width 3) height "solid" left)
   (rectangle (/ width 3) height "solid" middle)
   (rectangle (/ width 3) height "solid" right)))

(check-expect
 (tricolor 30 20 "blue" "white" "red")
 (beside (rectangle 10 20 "solid" "blue")
         (rectangle 10 20 "solid" "white")
         (rectangle 10 20 "solid" "red")))

(check-expect
 (tricolor 80 40 "green" "white" "orange")
 (beside (rectangle 80/3 40 "solid" "green")
         (rectangle 80/3 40 "solid" "white")
         (rectangle 80/3 40 "solid" "orange")))

; The flag of Cameroon is a tricolor (green, red, yellow)
; with a gold star centered over the red band.

(define (flag-of-cameroon width height)
 (beside (rectangle width height "solid" "green")
         (overlay (star (/ width 7) "solid" "gold")(rectangle width height "solid" "red"))
         (rectangle width height "solid" "yellow")))