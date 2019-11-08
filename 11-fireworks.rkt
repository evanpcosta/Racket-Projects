;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 11-fireworks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lecture 11: Pre-Abstraction
;;
;; Road map:
;;  1) Code reading
;;  2) List recursion

(require 2htdp/image)
(require 2htdp/universe)

;; Fireworks shoot upward, under the influence of gravity, for
;; some period of time, and then explode up to some radius.
;; While shooting upward as a single mortar, a firework has a position
;; and velocity, the time remaining until its explosion, and a list of
;; of “pyrotechnic stars” that will be released by the explosion.
;;
;; Once the fuse runs out, it explodes into that group of pyrotechnic
;; stars, each of which has its own position and velocity, as well color
;; and time before burning.


;;;;;
;;;;; DATA DEFINITIONS
;;;;;

; A Vel is (make-vel Number Number)
(define-struct vel (dx dy))
; interp. the velocity of an object in pixels/tick

; A PosnVel is (make-posnvel Posn Vel)
(define-struct posnvel (posn vel))
; interp. the position and velocity of an object

; A PStar is (make-pstar PosnVel Nat Color)
(define-struct pstar [posnvel ticks color])
; interp. one of the colored rocket fragments after exploding, where
; `ticks` gives the number of ticks remaining until it burns out.

; A ListOfVel is one of:
;  - '()
;  - (cons Vel ListOfVel)

; A Mortar is (make-mortar PosnVel Nat ListOfVel Nat Color)
(define-struct mortar [posnvel ticks pstar-vels pstar-ticks pstar-color])
; interp. a rocket before exploding, where `ticks` gives the number
; of ticks remaining before it explodes, `pstar-color` is the color
; of all its stars, and `pstar-vels` gives the initial velocities of
; all its stars relative to the mortar's velocity at the moment of
; explosion.

; A ListOfMortar is one of:
;  - '()
;  - (cons Mortar ListOfMortar)

; A ListOfPStar is one of:
;  - '()
;  - (cons PStar ListOfPStar)

; A Fireshow is (make-fs ListOfMortar ListOfPStar)
(define-struct fs [mortars pstars])


;;;;;
;;;;; CONSTANTS
;;;;;

; A Interval is (make-interval Integer Integer)
(define-struct interval [low high])
; interp. the closed interval of numbers from `low` to `high`.

(define WIDTH            800)
(define HEIGHT           600)
(define GRAVITY          1)  ; px/tick/tick
(define MORTAR-RADIUS    1)
(define MORTAR-COLOR     "yellow")
(define PSTAR-RADIUS     2)
(define PSTAR-COLORS     (list "white" "green" "yellow" "red" "blue"))

(define MORTAR-DX        (make-interval  -3   3))
(define MORTAR-DY        (make-interval -35 -25))
(define MORTAR-TICKS     (make-interval  35  45))
(define PSTAR-COUNT      (make-interval  10  80))
(define PSTAR-TICKS      (make-interval  10  20))
(define PSTAR-SPEED      (make-interval   5  15))  ; px/tick

(define MT               (empty-scene WIDTH HEIGHT "black"))


;;;;;
;;;;; DATA EXAMPLES
;;;;;

;; Each pair is before/after a tick:
(define POSNVEL0-1 (make-posnvel (make-posn 200 150)
                                 (make-vel 20 20)))
(define POSNVEL0-2 (make-posnvel (make-posn 220 170)
                                 (make-vel 20 (+ GRAVITY 20))))
(define POSNVEL1-1 (make-posnvel (make-posn 104 270)
                                 (make-vel -26 -40)))
(define POSNVEL1-2 (make-posnvel (make-posn 78 230)
                                 (make-vel -26 (+ GRAVITY -40))))
(define POSNVEL2-1 (make-posnvel (make-posn 104 270)
                                 (make-vel 24 -10)))
(define POSNVEL2-2 (make-posnvel (make-posn 128 260)
                                 (make-vel  24 (+ GRAVITY -10))))
(define POSNVEL3-1 (make-posnvel (make-posn 100 (+ 300 GRAVITY))
                                 (make-vel 4 (- -30 GRAVITY))))
(define POSNVEL3-2 (make-posnvel (make-posn 104 270)
                                 (make-vel 4 -30)))

(define PSTAR0-1 (make-pstar POSNVEL0-1  1 "white"))
(define PSTAR0-2 (make-pstar POSNVEL0-2  0 "white"))
(define PSTAR1-1 (make-pstar POSNVEL1-1 10 "green"))
(define PSTAR1-2 (make-pstar POSNVEL1-2  9 "green"))
(define PSTAR2-1 (make-pstar POSNVEL2-1 10 "green"))
(define PSTAR2-2 (make-pstar POSNVEL2-2  9 "green"))

(define MORTAR1-1 (make-mortar POSNVEL3-1 1
                               (list (make-vel -30 -10) (make-vel 20 20))
                               10 "green"))
(define MORTAR1-2 (make-mortar POSNVEL3-2 0
                               (list (make-vel -30 -10) (make-vel 20 20))
                               10 "green"))

(define SHOW-0 (make-fs '() '()))
(define SHOW-1 (make-fs (list MORTAR1-1) '()))
(define SHOW-2 (make-fs (list MORTAR1-2) '()))
(define SHOW-3 (make-fs '() (list PSTAR1-1 PSTAR2-1)))
(define SHOW-4 (make-fs '() (list PSTAR1-2 PSTAR2-2)))


;;;;;
;;;;; Big bang
;;;;;

; run : Any -> Fireshow
; Starts a firework show -- call with any argument.
;
; Example: (run 0)
#;
(define (run _dummy)
  (big-bang SHOW-0
     [on-tick   step-fireshow]
     [on-mouse  fireshow-handle-mouse]
     [to-draw   render-fireshow]
     [name      "Fireworks Show"]))


;
;
;                                               ;               ;
;                                   ;           ;               ;
;                                   ;                           ;
;     ;;;;    ; ;;;;;             ;;;;;;     ;;;;       ;;;;    ;    ;
;    ;    ;   ;;    ;;              ;           ;      ;    ;   ;   ;
;   ;      ;  ;      ;              ;           ;     ;         ; ;;
;   ;      ;  ;      ;              ;           ;     ;         ;;;
;   ;      ;  ;      ;  ;;;;;;;;    ;           ;     ;         ;; ;
;   ;      ;  ;      ;              ;           ;     ;         ;   ;
;   ;      ;  ;      ;              ;           ;     ;         ;    ;
;    ;    ;   ;      ;              ;           ;      ;    ;   ;    ;;
;     ;;;;    ;      ;               ;;;     ;;;;;;;    ;;;;    ;     ;;
;
;

; step-fireshow : Fireshow -> Fireshow
; Finds the next state of a fireworks show after one tick.
;
; Examples:
(check-expect (step-fireshow
               (make-fs (list MORTAR1-1 MORTAR1-2)
                        '()))
              (make-fs (list MORTAR1-2)
                       (list PSTAR1-1 PSTAR2-1)))
(check-expect (step-fireshow
               (make-fs (list MORTAR1-2)
                        (list PSTAR1-1 PSTAR2-1)))
              (make-fs '()
                       (list PSTAR1-1 PSTAR2-1 PSTAR1-2 PSTAR2-2)))
;
; Strategy: struct decomp (Fireshow)
(define (step-fireshow fs)
  (make-fs (step-mortars (fs-mortars fs))
           (append
            (explode-mortars (fs-mortars fs))
            (step-pstars (fs-pstars fs)))))


; step-mortars : ListOfMortar -> ListOfMortar
; Removes mortars that have exploded and then moves those that remain.
;
; Examples:
(check-expect (step-mortars (list MORTAR1-1 MORTAR1-1))
              (list MORTAR1-2 MORTAR1-2))
(check-expect (step-mortars (list MORTAR1-2 MORTAR1-2))
              '())
;
; Strategy: function composition
(define (step-mortars mortars)
  (move-mortars (remove-exploded-mortars mortars)))


; remove-exploded-mortars : ListOfMortar -> ListOfMortar
; Removes all mortars whose ticks remaining is 0.
;
; Examples:
(check-expect (remove-exploded-mortars (list MORTAR1-1)) (list MORTAR1-1))
(check-expect (remove-exploded-mortars (list MORTAR1-2)) '())
;
; Strategy: struct decomp (ListOfMortar) [RECURSION]
(define (remove-exploded-mortars mortars)
  (cond
    [(empty? mortars) '()]
    [else
     (cond
       [(mortar-alive? (first mortars))
        (cons (first mortars)
              (remove-exploded-mortars (rest mortars)))]
       [else  (remove-exploded-mortars (rest mortars))])]))


; move-mortars : ListOfMortar -> ListOfMortar
; Moves all mortars for a tick.
;
; Examples:
(check-expect (move-mortars (list MORTAR1-1)) (list MORTAR1-2))
(check-expect (move-mortars (list MORTAR1-1 MORTAR1-1))
              (list MORTAR1-2 MORTAR1-2))
;
; Strategy: struct decomp (ListOfMortar) [RECURSION]
(define (move-mortars mortars)
  (cond
    [(empty? mortars) '()]
    [else             (cons (move-mortar (first mortars))
                            (move-mortars (rest mortars)))]))


; move-mortar : Mortar -> Mortar
; Moves a mortar through space because of time.
;
; Examples:
(check-expect (move-mortar MORTAR1-1) MORTAR1-2)
;
; Strategy: struct decomp (Mortar)
(define (move-mortar mortar)
  (make-mortar (move-posnvel (mortar-posnvel mortar))
               (sub1 (mortar-ticks mortar))
               (mortar-pstar-vels mortar)
               (mortar-pstar-ticks mortar)
               (mortar-pstar-color mortar)))


; step-mortars : ListOfPStar -> ListOfPStar
; Moves all the stars for a tick, removing any that are finished
;
; Examples:
(check-expect (step-pstars (list PSTAR1-1 PSTAR2-1 PSTAR0-1))
              (list PSTAR1-2 PSTAR2-2 PSTAR0-2))
(check-expect (step-pstars (list PSTAR1-1 PSTAR2-1 PSTAR0-2))
              (list PSTAR1-2 PSTAR2-2))
;
; Strategy: function composition
(define (step-pstars pstars)
  (move-pstars (remove-finished-pstars pstars)))
  

; remove-finished-pstars : ListOfPStar -> ListOfPStar
; Removes all pstars whose ticks remaining is 0.
;
; Examples:
(check-expect (remove-finished-pstars (list PSTAR0-1)) (list PSTAR0-1))
(check-expect (remove-finished-pstars (list PSTAR0-2)) '())
;
; Strategy: struct decomp (ListOfPStar) [RECURSION]
(define (remove-finished-pstars pstars)
  (cond
    [(empty? pstars) '()]
    [else
     (cond
       [(pstar-alive? (first pstars))
        (cons (first pstars)
              (remove-finished-pstars (rest pstars)))]
       [else  (remove-finished-pstars (rest pstars))])]))


; move-pstars : ListOfPStar -> ListOfPStar
; Moves all pstars for a tick.
;
; Examples:
(check-expect (move-pstars (list PSTAR1-1)) (list PSTAR1-2))
(check-expect (move-pstars (list PSTAR1-1 PSTAR2-1)) (list PSTAR1-2 PSTAR2-2))
;
; Strategy: struct decomp (ListOfMortar) [RECURSION]
(define (move-pstars pstars)
  (cond
    [(empty? pstars) '()]
    [else            (cons (move-pstar (first pstars))
                           (move-pstars (rest pstars)))]))


; move-pstar : PStar -> PStar
; Advances a PStar in time.
;
; Examples:
(check-expect (move-pstar PSTAR1-1) PSTAR1-2)
(check-expect (move-pstar PSTAR2-1) PSTAR2-2)
;
; Strategy: struct decomp (PStar)
(define (move-pstar star)
  (make-pstar (move-posnvel (pstar-posnvel star))
              (sub1 (pstar-ticks star))
              (pstar-color star)))


; explode-mortars : ListOfMortar -> ListOfPStar
; Gets all the stars of all the exploding mortars.
;
; Examples:
(check-expect (explode-mortars (list MORTAR1-1)) '())
(check-expect (explode-mortars (list MORTAR1-2)) (list PSTAR1-1 PSTAR2-1))
(check-expect (explode-mortars (list MORTAR1-2 MORTAR1-2))
              (list PSTAR1-1 PSTAR2-1 PSTAR1-1 PSTAR2-1))
;
; Strategy: function composition
(define (explode-mortars mortars)
  (explode-mortars/append (find-exploded-mortars mortars) '()))


; find-exploded-mortars  : ListOfMortar -> ListOfMortar
; Returns all mortars whose ticks remaining are non-zero.
;
; Examples:
(check-expect (find-exploded-mortars (list MORTAR1-1)) '())
(check-expect (find-exploded-mortars (list MORTAR1-2)) (list MORTAR1-2))
;
; Strategy: struct decomp (ListOfMortar) [RECURSION]
(define (find-exploded-mortars mortars)
  (cond
    [(empty? mortars) '()]
    [else
     (cond
       [(mortar-exploded? (first mortars))
        (cons (first mortars)
              (find-exploded-mortars (rest mortars)))]
       [else  (find-exploded-mortars (rest mortars))])]))


; explode-mortars/append : ListOfMortar ListOfPStar -> ListOfPStar
; Explodes all the given mortars into stars and appends those stars
; to `pstars`.
;
; Examples:
(check-expect (explode-mortars/append '() (list PSTAR0-1))
              (list PSTAR0-1))
(check-expect (explode-mortars/append (list MORTAR1-2)
                                      (list PSTAR0-1))
              (list PSTAR1-1 PSTAR2-1 PSTAR0-1))
(check-expect (explode-mortars/append (list MORTAR1-2 MORTAR1-2)
                                      (list PSTAR0-1))
              (list PSTAR1-1 PSTAR2-1 PSTAR1-1 PSTAR2-1 PSTAR0-1))
;
; Strategy: struct decomp (ListOfMortar) [RECURSION]
(define (explode-mortars/append mortars pstars)
  (cond
    [(empty? mortars)
     pstars]
    [else
     (append (explode-mortar (first mortars))
             (explode-mortars/append (rest mortars) pstars))]))


; explode-mortar : Mortar -> ListOfPStar
; Gets the stars from one exploding mortar.
;
; Examples:
(check-expect (explode-mortar MORTAR1-2)
              (list PSTAR1-1 PSTAR2-1))

;
; Strategy: struct decomp
(define (explode-mortar mortar)
  (build-pstars (mortar-posnvel mortar)
                (mortar-pstar-ticks mortar)
                (mortar-pstar-color mortar)
                (mortar-pstar-vels mortar)))


; build-pstars : PosnVel Number Color ListOfVel -> ListOfPStar
; Builds a list of PStars whose positions are from `mortar-pv`, ticks and colors
; as given, and velocities are the velocities in `vels` added to the velocity in
; `mortar-pv`.
;
; Examples:
(check-expect (build-pstars (mortar-posnvel MORTAR1-2)
                            (mortar-pstar-ticks MORTAR1-2)
                            (mortar-pstar-color MORTAR1-2)
                            (mortar-pstar-vels MORTAR1-2))
              (list PSTAR1-1 PSTAR2-1 ))
;
; Strategy: struct decomp (ListOfVel) [RECURSION]
(define (build-pstars mortar-pv ticks color vels)
  (cond
    [(empty? vels) '()]
    [else
      (cons (make-pstar
              (move-posnvel-by-vel mortar-pv (first vels))
              ticks
              color)
            (build-pstars mortar-pv ticks color (rest vels)))]))


; mortar-alive? : Mortar -> Boolean
; Determines whether the given mortar has yet to explode.
;
; Examples:
(check-expect (mortar-alive? MORTAR1-1) #true)
(check-expect (mortar-alive? MORTAR1-2) #false)
;
; Strategy: struct decomp (Mortar)
(define (mortar-alive? mortar)
  (not (mortar-exploded? mortar)))


; mortar-exploded? : Mortar -> Boolean
; Determines whether the given mortar's time has run out.
;
; Examples:
(check-expect (mortar-exploded? MORTAR1-1) #false)
(check-expect (mortar-exploded? MORTAR1-2) #true)
;
; Strategy: struct decomp (Mortar)
(define (mortar-exploded? mortar)
  (zero? (mortar-ticks mortar)))


; pstar-alive? : PStar -> Boolean
; Determines whether the given star's is still going.
;
; Examples:
(check-expect (pstar-alive? PSTAR0-1) #true)
(check-expect (pstar-alive? PSTAR0-2) #false)
;
; Strategy: struct decomp (PStar)
(define (pstar-alive? star)
  (positive? (pstar-ticks star)))


; move-posnvel : PosnVel -> PosnVel
; Updates a PosnVel for one tick.
;
; Examples:
(check-expect (move-posnvel POSNVEL1-1) POSNVEL1-2)
(check-expect (move-posnvel POSNVEL2-1) POSNVEL2-2)
;
; Strategy: struct decomp (PosnVel)
(define (move-posnvel posnvel)
  (make-posnvel (step-posn-by-vel (posnvel-posn posnvel) (posnvel-vel posnvel))
                (+/vel (posnvel-vel posnvel) (make-vel 0 GRAVITY))))


; move-posnvel-by-vel : PosnVel Vel -> PosnVel
; Updates a PosnVel by adding the given velocity to its velocity.
;
; Examples:
(check-expect
 (move-posnvel-by-vel (make-posnvel (make-posn 200 300) (make-vel 4 7))
                   (make-vel -1 3))
 (make-posnvel (make-posn 200 300) (make-vel 3 10)))
;
; Strategy: struct decomp (PosnVel)
(define (move-posnvel-by-vel posnvel vel)
  (make-posnvel (posnvel-posn posnvel)
                (+/vel (posnvel-vel posnvel) vel)))



; step-posn-by-vel : Posn Vel -> Posn
; Adds the velocity to the position to get the new position.
;
; Examples:
(check-expect (step-posn-by-vel (make-posn 3 4) (make-vel 10 20))
              (make-posn 13 24))
;
; Strategy: struct decomp (Posn and Vel)
(define (step-posn-by-vel posn vel)
  (make-posn (+ (posn-x posn) (vel-dx vel))
             (+ (posn-y posn) (vel-dy vel))))


; +/vel : Vel Vel -> Posn
; Adds two velocities.
;
; Examples:
(check-expect (+/vel (make-vel 3 4) (make-vel 10 20))
              (make-vel 13 24))
;
; Strategy: struct decomp (Vel and Vel)
(define (+/vel vel1 vel2)
  (make-vel (+ (vel-dx vel1) (vel-dx vel2))
            (+ (vel-dy vel1) (vel-dy vel2))))


;
;
;
;                                        ;
;     ;                                  ;
;     ;                                  ;
;   ;;;;;;      ;;;;                ;;;; ;    ; ;;;     ;;;;;  ;        ;
;     ;        ;    ;              ;    ;;    ;;   ;   ;    ;; ;        ;
;     ;       ;      ;            ;      ;    ;              ;  ;      ;
;     ;       ;      ;            ;      ;    ;         ;;;;;;  ;  ;;  ;
;     ;       ;      ;  ;;;;;;;;  ;      ;    ;       ;;     ;  ;  ;;  ;
;     ;       ;      ;            ;      ;    ;       ;      ;  ;  ;;  ;
;     ;       ;      ;            ;      ;    ;       ;     ;;   ;;  ;;
;     ;        ;    ;              ;    ;;    ;       ;;   ;;;   ;;  ;;
;      ;;;      ;;;;                ;;;; ;    ;        ;;;;; ;   ;;  ;;
;
;


; render-fire-show : Fireshow -> Scene
; To draw all the fireworks in the firework show.
;
; Examples:
(check-expect (render-fireshow
               (make-fs (list MORTAR1-1)
                        (list PSTAR0-1 PSTAR1-1 PSTAR2-1)))
              (render-pstar
               PSTAR0-1
               (render-pstar
                PSTAR1-1
                (render-pstar
                 PSTAR2-1
                 (render-mortar
                  MORTAR1-1
                  MT)))))
;
; Strategy: struct decomp (Fireshow)
(define (render-fireshow fs)
  (render-pstars (fs-pstars fs)
                 (render-mortars (fs-mortars fs) MT)))


; render-pstars : ListOfPStar Scene -> Scene
; Renders all the stars in the list into the given scene.
;
; Examples:
(check-expect (render-pstars '() MT) MT)
(check-expect (render-pstars (list PSTAR1-1 PSTAR2-1) MT)
              (render-pstar PSTAR1-1
                            (render-pstar PSTAR2-1 MT)))
;
; Strategy: struct decomp (ListOfPStar) [RECURSION]
(define (render-pstars pstars scene)
  (cond
    [(empty? pstars)  scene]
    [else             (render-pstar (first pstars)
                                    (render-pstars (rest pstars) scene))]))


; render-pstar : PStar Scene -> Scene
; Renders one star into the given scene.
;
; Examples:
(check-expect (render-pstar (make-pstar POSNVEL1-1 3 "green") MT)
              (place-image/posn (circle PSTAR-RADIUS "solid" "green")
                                (posnvel-posn POSNVEL1-1)
                                MT))
(check-expect (render-pstar (make-pstar POSNVEL2-2 5 "blue") MT)
              (place-image/posn (circle PSTAR-RADIUS "solid" "blue")
                                (posnvel-posn POSNVEL2-2)
                                MT))
;
; Strategy: struct decomp (Mortar)
(define (render-pstar star scene)
  (place-circle/posnvel PSTAR-RADIUS
                        (pstar-color star)
                        (pstar-posnvel star)
                        scene))


; render-mortars : ListOfMortar Scene -> Scene
; Renders all the mortars in the list into the given scene.
;
; Examples:
(check-expect (render-mortars '() MT) MT)
(check-expect (render-mortars (list MORTAR1-1 MORTAR1-2) MT)
              (render-mortar MORTAR1-1
                             (render-mortar MORTAR1-2 MT)))
;
; Strategy: struct decomp (ListOfMortar) [RECURSION]
(define (render-mortars mortars scene)
  (cond
    [(empty? mortars)  scene]
    [else              (render-mortar (first mortars)
                                      (render-mortars (rest mortars) scene))]))


; render-mortar : Mortar Scene -> Scene
; Renders one mortar into the given scene.
;
; Examples:
(check-expect (render-mortar MORTAR1-1 MT)
              (place-image/posn (circle MORTAR-RADIUS "solid" MORTAR-COLOR)
                                (posnvel-posn (mortar-posnvel MORTAR1-1))
                                MT))
;
; Strategy: struct decomp (Mortar)
(define (render-mortar mortar scene)
  (place-circle/posnvel MORTAR-RADIUS
                        MORTAR-COLOR
                        (mortar-posnvel mortar)
                        scene))


; place-circle/posnvel : Number Color PosnVel Scene -> Scene
; Places a circle with the given radius and color at the position of
; the given PosnVel.
;
; Examples:
(check-expect (place-circle/posnvel 5 "blue" POSNVEL1-1 MT)
              (place-image/posn (circle 5 "solid" "blue")
                                (posnvel-posn POSNVEL1-1)
                                MT))
;
; Strategy: struct decomp (PosnVel)
(define (place-circle/posnvel radius color posnvel scene)
  (place-image/posn (circle radius "solid" color)
                    (posnvel-posn posnvel)
                    scene))


; place-image/posn : Image Posn Scene -> Scene
; Places the image at the given position.
;
; Examples:
(check-expect (place-image/posn (circle 5 "solid" "red") (make-posn 100 150) MT)
              (place-image (circle 5 "solid" "red") 100 150 MT))
;
; Strategy: struct decomp (Posn)
(define (place-image/posn image posn scene)
  (place-image image (posn-x posn) (posn-y posn) scene))


;
;
;
;
;
;
;     ;;;    ; ;;;;            ;;;;;;     ;;;    ;     ;   ;;;;;    ;;;;
;    ;   ;   ;;   ;;           ;  ;  ;   ;   ;   ;     ;  ;     ;   ;   ;
;   ;     ;  ;     ;           ;  ;  ;  ;     ;  ;     ;  ;        ;    ;;
;   ;     ;  ;     ;  ;;;;;;;; ;  ;  ;  ;     ;  ;     ;  ;;;;     ;;;;;;;
;   ;     ;  ;     ;           ;  ;  ;  ;     ;  ;     ;     ;;;;  ;
;   ;     ;  ;     ;           ;  ;  ;  ;     ;  ;     ;        ;  ;
;    ;   ;   ;     ;           ;  ;  ;   ;   ;   ;;   ;;  ;     ;   ;
;     ;;;    ;     ;           ;  ;  ;    ;;;     ;;;; ;   ;;;;;     ;;;;
;
;


;; Let's allow the user to add new fireworks to the show by clicking
;; the mouse.  This should add a firework at the clicked x coordinate
;; at the bottom of the scene, with other components randomized.


; fireshow-handle-mouse : Fireshow Number Number MouseEvent -> Fireshow
; Response to mouse clicks by creating new mortars.
;
; Examples:
;  - (fireshow-handle-mouse (make-fs {some mortars} {some stars})
;                           45 342 "button-up"")
;    (make-fs (cons {mortar at 45, bottom of screen} {some mortars})
;             {some stars})
;
;  - (fireshow-handle-mouse (make-fs {some mortars} {some stars})
;                           45 342 "drag"")
;    (make-fs {some mortars} {some stars})
;
; Strategy: struct decomp (MouseEvent)
(define (fireshow-handle-mouse fs x _y me)
  (cond
    [(string=? me "button-up") (add-mortar (random-mortar x) fs)]
    [else                      fs]))

(check-expect (fireshow-handle-mouse SHOW-1 48 300 "move")
              SHOW-1)
(check-expect
 (add1 (length (fs-mortars (fireshow-handle-mouse SHOW-1 48 300 "move"))))
 (length (fs-mortars (fireshow-handle-mouse SHOW-1 48 300 "button-up"))))


; add-mortar : Mortar Fireshow -> Fireshow
; Adds the mortar to the fireshow's list of mortars.
;
; Examples:
(check-expect (add-mortar MORTAR1-1 (make-fs '() (list PSTAR1-1 PSTAR2-1)))
              (make-fs (list MORTAR1-1) (list PSTAR1-1 PSTAR2-1)))
(check-expect (add-mortar MORTAR1-1
                          (make-fs (list MORTAR1-2) (list PSTAR1-1 PSTAR2-1)))
              (make-fs (list MORTAR1-1 MORTAR1-2) (list PSTAR1-1 PSTAR2-1)))
;
; Strategy: struct decomp (Fireshow)
(define (add-mortar mortar fs)
  (make-fs (cons mortar (fs-mortars fs))
           (fs-pstars fs)))


; random-mortar : Number -> Mortar
; Creates a random mortar at the given x coordinate and bottom of the screen.
;
; Examples:
;  - (random-mortar 78) => {mortar at (78, HEIGHT)}
;  - (random-mortar 300) => {mortar at (300, HEIGHT)}
;
; Strategy: function composition
(define (random-mortar x)
  (make-mortar (make-posnvel (make-posn x HEIGHT)
                             (make-vel (random-from-interval MORTAR-DX)
                                       (random-from-interval MORTAR-DY)))
               (random-from-interval MORTAR-TICKS)
               (random-vel-list (random-from-interval PSTAR-COUNT)
                                PSTAR-SPEED)
               (random-from-interval PSTAR-TICKS)
               (random-from-list PSTAR-COLORS)))

(check-expect (posnvel-posn (mortar-posnvel (random-mortar 98)))
              (make-posn 98 HEIGHT))


; random-vel-list : Nat Interval -> ListOfVel
; Generates a list of `n` random velocities whose speed is in `interval`
;
; Examples:
;  - (random-vel-list 0 (make-interval 5 9)) => '()
;  - (random-vel-list 3 (make-interval 0 0))
;      => (list (make-vel 0 0) (make-vel 0 0) (make-vel 0 0))
;  - (random-vel-list 4 (make-interval 3 5)) =>
;      => a list of 4 random velocities whose speeds are all between 3 and 5
;
; Strategy: struct decomp (Nat) [RECURSION]
(define (random-vel-list n interval)
  (cond
    [(zero? n) '()]
    [else      (cons (polar-velocity (random-from-interval interval)
                                     (random-radians 0))
                     (random-vel-list (sub1 n) interval))]))

(check-expect (random-vel-list 0 8)
              '())
(check-expect (length (random-vel-list 6 (make-interval 5 8)))
              6)
(check-expect (random-vel-list 2 (make-interval 0 0))
              (list (make-vel 0 0) (make-vel 0 0)))
(check-within
 (vel-magnitude (first (random-vel-list 1 (make-interval 19.2 19.2))))
 19.2 1E-5)


; vel-magnitude : Vel -> Number
; Computes the magnitude of a velocity (speed).
;
; Examples:
(check-within (vel-magnitude (make-vel 7 0))    7 1E-5)
(check-within (vel-magnitude (make-vel 12 -5)) 13 1E-5)
;
; Strategy: struct decomp (Vel)
(define (vel-magnitude vel)
 (sqrt (+ (sqr (vel-dx vel)) (sqr (vel-dy vel)))))


; polar-velocity : Number Number -> Vel
; Constructs a `Vel` from the unoriented speed and a direction (in radians).
;
; Examples:
(check-within (polar-velocity 10 0)
              (make-vel 10 0)
              1E-5)
(check-within (polar-velocity 10 (/ pi 2))
              (make-vel 0 10)
              1E-5)
(check-within (polar-velocity 10 (/ pi 4))
              (make-vel (* 5 (sqrt 2)) (* 5 (sqrt 2)))
              1E-5)

(define (polar-velocity speed angle)
  (make-vel (* speed (cos angle))
            (* speed (sin angle))))


;
;
;
;                                        ;
;                                        ;
;                                        ;
;     ; ;;;     ;;;;;   ; ;;;;;     ;;;; ;    ;;;;     ;;;;
;     ;;   ;   ;    ;;  ;;    ;;   ;    ;;   ;    ;    ; ; ;
;     ;              ;  ;      ;  ;      ;  ;      ;   ; ; ;
;     ;         ;;;;;;  ;      ;  ;      ;  ;      ;   ; ; ;
;     ;       ;;     ;  ;      ;  ;      ;  ;      ;   ; ; ;
;     ;       ;      ;  ;      ;  ;      ;  ;      ;   ; ; ;
;     ;       ;     ;;  ;      ;  ;      ;  ;      ;   ; ; ;
;     ;       ;;   ;;;  ;      ;   ;    ;;   ;    ;    ; ; ;
;     ;        ;;;;; ;  ;      ;    ;;;; ;    ;;;;     ; ; ;
;
;

; random-radians : Any -> Number
; Returns a random angle in radians.
;
; Examples:
(check-expect (<= 0 (random-radians 0) (* 2 pi)) #true)
;
; Strategy: domain knowledge
(define (random-radians _dummy)
  (* pi 1/180 (random 360)))


; A ListOfColor is one of:
;  - '()
;  - (cons Color ListOfColor)


; random-from-list : ListOfColor -> Color
; Chooses a random color from a list of colors.
;
; Examples:
(check-expect (random-from-list (list "red")) "red")
(check-member-of
  (random-from-list (list "red" "green" "blue" "yellow"))
  "red" "green" "blue" "yellow")
;
; Strategy: domain knowledge
(define (random-from-list lst)
  (list-ref lst (random (length lst))))


; random-from-interval : Range -> Integer
; Chooses a random number from the given closed interval.
;
; Examples:
;  - (random-from-interval (make-interval 4 4)) => 4
;  - (random-from-interval (make-interval 4 5)) => 4 or 5
;  - (random-from-interval (make-interval 4 7)) => 4, 5, 6, or 7
;
; Strategy: struct decomp (Interval)
(define (random-from-interval i)
  (random-between (interval-low i) (interval-high i)))

(check-expect (random-from-interval (make-interval 8 8)) 8)
(check-random (random-from-interval (make-interval 5 10))
              (+ 5 (random 6)))


; random-between : Integer Integer -> Integer
; Chooses a random number between `low` and `high` (inclusive).
;
; Examples:
;  - (random-between 5 10) => 5, 6, 7, 8, 9, or 10
;  - (random-between -1 1) => -1, 0, or 1
;
; Strategy: domain knowledge
(define (random-between low high)
  (+ low (random (add1 (- high low)))))

