;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 08-aquarium) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EECS 111 Lecture 8: Medium Data
;;
;; Road map:
;;  1) One
;;  2) Two
;;  *) Many

(require 2htdp/image)
(require 2htdp/universe)


;
;
;
;     ;;;;
;    ;    ;
;    ;    ;;
;   ;      ;  ; ;;;;;     ;;;;
;   ;      ;  ;;    ;;   ;;   ;
;   ;      ;  ;      ;  ;;    ;;
;   ;      ;  ;      ;  ;;     ;
;   ;      ;  ;      ;  ;;;;;;;;
;   ;      ;  ;      ;  ;;
;   ;;    ;;  ;      ;  ;;
;    ;    ;   ;      ;   ;;    ;
;     ;;;;    ;      ;    ;;;;;;
;
; One Fish in an Aquarium
; (0 to 1 is the hardest)
;


; A Fish is one of:
;  - (make-goldfish Number)
;  - (make-angelfish Number Color)
;  - (make-zebra-danio Number ZebraDanioType Color Color)
; interp. if `f` is a Fish, then it must be one of these cases:
;  - if (goldfish? f), then (goldfish-size f) is the size of the goldfish;
;  - if (angelfish? f), then all of:
;     - (angelfish-size f) is its size and
;     - (angelfish-color f) is its color; or
;  - if (zebra-danio? f), then all of:
;     - (zebra-danio-size f) is its size,
;     - (zebra-danio-type f) is its type (defined next),
;     - (zebra-danio-body f) is its main color, and
;     - (zebra-danio-stripe f) is the color of its stripes.
;
; where
;
; A ZebraDanioType is one of:
;  - "double"
;  - "triple"
;  - "quadruple"
(define-struct goldfish [size])
(define-struct angelfish [size color])
(define-struct zebra-danio [size type body stripe])

; process-fish : Fish ... -> ...
; Template for Fish.
#;
(define (process-fish fish ...)
  (cond
    [(goldfish? fish)  ... (goldfish-size fish) ...]
    [(angelfish? fish) ... (angelfish-size fish) ...
                       ... (angelfish-color fish) ...]
    [else              ... (zebra-danio-size fish) ...
                       ... (zebra-danio-type fish) ...
                       ... (zebra-danio-body fish) ...
                       ... (zebra-danio-stripe fish) ...]))

; process-zebra-danio-type : ZebraDanioType ... -> ...
; Template for ZebraDanioType.
#;
(define (process-zebra-danio-type zdt ...)
  (cond
    [(string=? zdt "double")      ...]
    [(string=? zdt "triple")      ...]
    [(string=? zdt "quadruple")   ...]))

;; Fish examples:

(define A-GOLDFISH (make-goldfish 100))
(define AN-ANGELFISH (make-angelfish 150 "silver"))
(define A-ZEBRA-DANIO (make-zebra-danio 75 "triple" "green" "white"))


; fish-size : Fish -> Number
; Finds the size of a fish.
;
; Examples:
(check-expect (fish-size A-GOLDFISH)    100)
(check-expect (fish-size AN-ANGELFISH)  150)
(check-expect (fish-size A-ZEBRA-DANIO) 75)
;
; Strategy: struct decomp (Fish)
(define (fish-size fish)
  (cond
    [(goldfish? fish)  (goldfish-size fish)]
    [(angelfish? fish) (angelfish-size fish)]
    [else              (zebra-danio-size fish)]))


; scale-fish : Number Fish -> Fish
; Scales the size of `fish` by `factor`.
;
; Examples:
(check-expect (scale-fish 2 (make-goldfish 50))
              (make-goldfish 100))
(check-expect (fish-size (scale-fish 4 AN-ANGELFISH))
              (* 4 (fish-size AN-ANGELFISH)))
(check-expect (fish-size (scale-fish 1/3 A-ZEBRA-DANIO))
              (* 1/3 (fish-size A-ZEBRA-DANIO)))
;
; Strategy: struct decomp (Fish)
(define (scale-fish factor fish)
  (cond
    [(goldfish? fish)  (make-goldfish (* factor (goldfish-size fish)))]
    [(angelfish? fish) (make-angelfish (* factor (angelfish-size fish))
                                       (angelfish-color fish))]
    [else              (make-zebra-danio (* factor (zebra-danio-size fish))
                                         (zebra-danio-type fish)
                                         (zebra-danio-body fish)
                                         (zebra-danio-stripe fish))]))


; fish->image : Fish -> Image
; Renders the fish as an image, facing right
;
; Examples:
;  - (fish->image (make-goldfish 100))
;      => a 100-pixel goldfish facing right
;  - (fish->image (make-angelfish 75 "red"))
;      => a 75-pixel, red angelfish facing left
;
; Strategy: struct decomp (Fish)
(define (fish->image fish)
  (cond
    [(goldfish? fish)
     (render-goldfish (* GOLDFISH-SCALE-X (goldfish-size fish))
                      (* GOLDFISH-SCALE-Y (goldfish-size fish))
                      GOLDFISH-COLOR)]
    [(angelfish? fish)
     (render-angelfish (* ANGELFISH-SCALE (angelfish-size fish))
                       (angelfish-color fish))]
    [else
     (render-zebra-danio (* ZEBRA-DANIO-SCALE-X (zebra-danio-size fish))
                         (* ZEBRA-DANIO-SCALE-Y (zebra-danio-size fish))
                         (zebra-danio-type fish)
                         (zebra-danio-body fish)
                         (zebra-danio-stripe fish))]))

(check-expect (fish->image (make-goldfish 100))
              (render-goldfish (* GOLDFISH-SCALE-X 100)
                               (* GOLDFISH-SCALE-Y 100)
                               GOLDFISH-COLOR))
(check-expect (fish->image (make-angelfish 75 "red"))
              (render-angelfish (* ANGELFISH-SCALE 75)
                                "red"))
(check-expect (fish->image (make-zebra-danio
                            40 "double" "black" "silver"))
              (render-zebra-danio (* ZEBRA-DANIO-SCALE-X 40)
                                  (* ZEBRA-DANIO-SCALE-Y 40)
                                  "double"
                                  "black"
                                  "silver"))


; scale-to : PositiveNumber PositionNumber Image -> Image
; Scales `img` to have dimensions `w` by `h`.
;
; Examples:
(check-expect (scale-to 100 200 (rectangle 300 50 "solid" "green"))
              (rectangle 100 200 "solid" "green"))
(check-expect (scale-to 90 75 (circle 10 "solid" "blue"))
              (ellipse 90 75 "solid" "blue"))
;
; Strategy: domain knowledge (geometry)
(define (scale-to w h img)
  (scale/xy (/ w (image-width img))
            (/ h (image-height img))
            img))


;; Where shall we keep our fish?


; An FishState is (make-fs Fish Posn Number)
; interp. if `fs` is an FishState then all of:
;  - (fs-fish fs) is the fish in the configuration,
;  - (fs-posn fs) is the position of the fish, and
;  - (fs-dx fs) is the horizontal velocity of the fish.
(define-struct fs [fish posn dx])

; process-fs : FishState ... -> ...
; Template for FishState.
#;
(define (process-fs fs ...)
  ... (fs-fish fs) ...
  ... (fs-posn fs) ...
  ... (fs-dx fs) ...)

#|
Note: Posn is built into BSL. It's defined like this:

; A Posn is (make-posn Number Number)
(define-struct posn [x y])
|#

;;; FishState examples

(define GOLDFISH-STATE (make-fs A-GOLDFISH (make-posn 100 200) -8))
(define ANGELFISH-STATE (make-fs AN-ANGELFISH (make-posn 150 230) 5))
(define ZEBRA-DANIO-STATE (make-fs A-ZEBRA-DANIO (make-posn 300 300) 15))

;;; FishState world constants

(define USE-BITMAP      #true)

(define AQUARIUM-WIDTH  640)
(define AQUARIUM-HEIGHT 480)
(define AQUARIUM-COLOR  "darkblue")
(define AQUARIUM-MARGIN 30)
(define GROW-RATE       4/3)
(define SHRINK-RATE     (/ 1 GROW-RATE))

(define OLD-AQUARIUM-BG
  (empty-scene AQUARIUM-WIDTH AQUARIUM-HEIGHT AQUARIUM-COLOR))
(define AQUARIUM-BG
  (scale-to
   AQUARIUM-WIDTH AQUARIUM-HEIGHT
   (bitmap/url "https://i.imgur.com/ewiIcgV.jpg")))

(define FISH-STATE-DX0    5)
(define FISH-STATE-POSN0  (make-posn 0 (* 2/3 AQUARIUM-HEIGHT)))


; start1 : Fish -> FishState
; Big-bang runner for FishState; takes one fish to
; start.
#;
(define (start1 fish)
  (big-bang (make-fs fish FISH-STATE-POSN0 FISH-STATE-DX0)
    [to-draw  fs-to-draw]
    [on-tick  fs-on-tick]
    [on-mouse fs-on-mouse]))


;;;
;;; RENDERING
;;;

; used in several tests below
(define TEST-IMAGE (beside (circle 60 "solid" "blue")
                           (circle 60 "solid" "green")))


; fs-to-draw : FishState -> Image
; Renders the fish configuration as a full scene.
;
; Examples:
;  - (fs-to-draw (make-fs A-GOLDFISH (make-posn 50 90) 10))
;    => aquarium scene with goldfish at (50, 90) facing right
;  - (fs-to-draw (make-fs AN-ANGELFISH (make-posn 500 250) -10))
;    => aquarium scene with an angelfish at (500, 250) facing left
;
; Strategy: function composition
(define (fs-to-draw fs)
  (place-fs fs AQUARIUM-BG))

(check-expect (fs-to-draw GOLDFISH-STATE)
              (place-fs GOLDFISH-STATE AQUARIUM-BG))


; place-fs : FishState Image -> Image
; Superimposes an image of the fish of `fs` onto `scene`.
;
; Examples:
;  - (place-fs (make-fs A-GOLDFISH (make-posn 50 90) 10)
;              (empty-scene 500 500))
;    => empty scene with goldfish at (50, 90) facing right
;  - (place-fs (make-fs AN-ANGELFISH (make-posn 500 250) -10)
;              AQUARIUM-BG)
;    => aquarium scene with an angelfish at (500, 250) facing left
;
; Strategy: function composition
(define (place-fs fs scene)
  (place-image/posn (flip-h-if (negative? (fs-dx fs))
                               (fish->image (fs-fish fs)))
                    (fs-posn fs)
                    scene))

(check-expect (place-fs (make-fs A-GOLDFISH (make-posn 100 50) 10)
                        AQUARIUM-BG)
              (place-image/posn (fish->image A-GOLDFISH)
                                (make-posn 100 50)
                                AQUARIUM-BG))
(check-expect (place-fs (make-fs A-GOLDFISH (make-posn 100 50) -10)
                        (empty-scene 500 500))
              (place-image/posn (flip-horizontal (fish->image A-GOLDFISH))
                                (make-posn 100 50)
                                (empty-scene 500 500)))


; flip-h-if : Boolean Image -> Image
; Returns`img`, flipped horizontally if `flip?` is true,
;
; Examples:
(check-expect (flip-h-if #false TEST-IMAGE) TEST-IMAGE)
(check-expect (flip-h-if #true  TEST-IMAGE) (flip-horizontal TEST-IMAGE))
;
; Strategy: struct decomp (Boolean)
(define (flip-h-if flip? img)
  (cond
    [flip? (flip-horizontal img)]
    [else  img]))


; place-image/posn : Image Posn Image -> Image
; Places `image`'s top-left at `position` in `scene`.
;
; Examples:
(check-expect (place-image/posn TEST-IMAGE (make-posn 8 4) AQUARIUM-BG)
              (overlay/align/offset "left" "top" TEST-IMAGE -8 -4 AQUARIUM-BG))
(check-expect (place-image/posn (square 100 "solid" "green")
                                (make-posn 350 100)
                                (empty-scene 400 400))
              (overlay/align/offset "right" "top"
                                    (rectangle 50 100 "solid" "green")
                                    0 -100
                                    (empty-scene 400 400)))
;
; Strategy: struct decomp (Posn)
(define (place-image/posn image position scene)
  (place-image/align image
                     (posn-x position) (posn-y position)
                     "left" "top"
                     scene))


;;;
;;; TICK HANDLING
;;;

; fs-on-tick : FishState -> FishState
; Updates the fish config to reflect the passage of time.
;
; Examples:
;  - Fish moving right:
(check-expect (fs-on-tick (make-fs A-GOLDFISH (make-posn 50 100) 10))
              (make-fs A-GOLDFISH (make-posn 60 100) 10))
;  - Fish moving left:
(check-expect (fs-on-tick (make-fs A-GOLDFISH (make-posn 50 100) -10))
              (make-fs A-GOLDFISH (make-posn 40 100) -10))
;  - Fish reverses from left to right:
(check-expect (fs-on-tick (make-fs A-GOLDFISH (make-posn 5 100) -10))
              (make-fs A-GOLDFISH (make-posn 15 100) 10))
;
; Strategy: decision tree
(define (fs-on-tick fs)
  (cond
    [(near-edge? fs) (move-fs-fish (reflect-fs-fish fs))]
    [else            (move-fs-fish fs)]))


; near-edge? : FishState -> Boolean
; Checks whether the fish is currently near the edge of the aquarium.
;
; Examples:
;  - Fish moving left, far from left edge:
(check-expect (near-edge? (make-fs A-GOLDFISH (make-posn 150 100) -10))
              #false)
;  - Fish moving left, near right edge:
(check-expect (near-edge? (make-fs A-GOLDFISH (make-posn 5 100) -10))
              true)
;
; Strategy: struct decomp (FishState)
(define (near-edge? fs)
  (near-edge?/parts (fs-fish fs) (fs-posn fs) (fs-dx fs)))


; near-edge?/parts : Fish Posn Number -> Boolean
; Checks whether `fish` at position `p` swimming with x velocity `dx` is
; getting close to an edge of the aquarium.
;
; Examples: Same as for `near-edge?`
;
; Strategy: decision tree
(define (near-edge?/parts fish p dx)
  (cond
    [(negative? dx)  (< (posn-x p) AQUARIUM-MARGIN)]
    [(positive? dx)  (> (+ (posn-x p) (fish-size fish))
                        (- AQUARIUM-WIDTH AQUARIUM-MARGIN))]
    [else            #false]))

(check-expect (near-edge?/parts A-GOLDFISH (make-posn 100 45) 0)
              #false)


; reflect-fs-fish : FishState -> FishState
; Turns the fish around left-to-right or right-to-left.
;
; Examples:
(check-expect (reflect-fs-fish (make-fs AN-ANGELFISH (make-posn 10 10) 7))
              (make-fs AN-ANGELFISH (make-posn 10 10) -7))
(check-expect (reflect-fs-fish (make-fs AN-ANGELFISH (make-posn 10 10) -8))
              (make-fs AN-ANGELFISH (make-posn 10 10) 8))
;
; Strategy: struct decomp (Fish)
(define (reflect-fs-fish fs)
  (make-fs (fs-fish fs)
           (fs-posn fs)
           (- (fs-dx fs))))


; move-fs-fish : FishState -> FishState
; Moves the fish in the direction it's currently going.
;
; Examples:
;  - Fish moving right:
(check-expect (move-fs-fish (make-fs A-ZEBRA-DANIO (make-posn 50 23) 12))
              (make-fs A-ZEBRA-DANIO (make-posn 62 23) 12))
;  - Fish moving left:
(check-expect (move-fs-fish (make-fs A-ZEBRA-DANIO (make-posn 50 23) -12))
              (make-fs A-ZEBRA-DANIO (make-posn 38 23) -12))
;
; Strategy: struct decomp (Fish)
(define (move-fs-fish fs)
  (make-fs (fs-fish fs)
           (shift-posn-x (fs-dx fs) (fs-posn fs))
           (fs-dx fs)))


; shift-posn-x : Number Posn -> Posn
; Adds `x` to the x coordinate of position `p`.
;
; Examples:
(check-expect (shift-posn-x 5 (make-posn 34 9)) (make-posn 39 9))
(check-expect (shift-posn-x -5 (make-posn 34 9)) (make-posn 29 9))
;
; Strategy: struct decomp (Posn)
(define (shift-posn-x dx p)
  (make-posn (+ dx (posn-x p)) (posn-y p)))


;;;
;;; MOUSE HANDLING
;;;

; fs-on-mouse : FishState Number Number MouseEvent -> FishState
; Handles mouse events to allow feeding the fish.
;
; Examples:
; Assume fish of size 100 at (100, 400). Then:
;  - click at (150, 200) (above fish) => fish size increases
;  - click at (150, 500) (below fish) => fish size decreases
;  - click at (50, 200) (behind fish) => fish size decreases
;  - move at (150, 200) => no change
;
; Strategy: struct decomp (MouseEvent)
(define (fs-on-mouse fs x y me)
  (cond
    [(string=? me "button-down") (handle-click fs x y)]
    [else fs]))

(check-expect
 (fs-on-mouse (make-fs (make-goldfish 100) (make-posn 100 400) 10)
               150 200 "button-down")
 (make-fs (make-goldfish (* GROW-RATE 100)) (make-posn 100 400) 10))
(check-expect
 (fs-on-mouse (make-fs (make-goldfish 100) (make-posn 100 400) 10)
               150 500 "button-down")
 (make-fs (make-goldfish (* SHRINK-RATE 100)) (make-posn 100 400) 10))
(check-expect
 (fs-on-mouse (make-fs (make-goldfish 100) (make-posn 100 400) 10)
               50 200 "button-down")
 (make-fs (make-goldfish (* SHRINK-RATE 100)) (make-posn 100 400) 10))
(check-expect
 (fs-on-mouse (make-fs (make-goldfish 100) (make-posn 100 400) 10)
               50 200 "button-up")
 (make-fs (make-goldfish 100) (make-posn 100 400) 10))


; handle-click : FishState Number Number -> FishState
; Given the positon of a click, grows the fish if the click is above it
; and shrinks otherwise.
;
; Examples:
; Assume fish of size 100 at (100, 400). Then:
;  - at (150, 200) (above fish) => fish size increases
;  - at (150, 500) (below fish) => fish size decreases
;  - at (50, 200) (behind fish) => fish size decreases
;
; Strategy: decision tree
(define (handle-click fs x y)
  (cond
    [(above-fs-fish? x y fs)  (scale-fs-fish GROW-RATE fs)]
    [else                     (scale-fs-fish SHRINK-RATE fs)]))


; scale-fs-fish : Number FishState -> FishState
; Scales the size of the fish in the aquarium.
;
; Examples:
;  - scale by 3 an fish config containing a size 50 goldfish
;     => the same but fish has size 150
;  - scale by /14 an fish config containing a size 100, yellow angelfish
;     => the same but fish has size 25
;
; Strategy: struct decomp (FishState)
(define (scale-fs-fish factor fs)
  (make-fs (scale-fish factor (fs-fish fs))
           (fs-posn fs)
           (fs-dx fs)))

(check-expect
 (scale-fs-fish 3 (make-fs (make-goldfish 50)
                           (make-posn 100 100) 8))
 (make-fs (make-goldfish 150) (make-posn 100 100) 8))
(check-expect
 (scale-fs-fish 3 (make-fs (make-angelfish 50 "yellow")
                           (make-posn 100 100) 8))
 (make-fs (make-angelfish 150 "yellow") (make-posn 100 100) 8))


; above-fs-fish? : Number Number FishState -> Boolean
; Is the position (x, y) above the fish in `fs`?
;
; Examples:
;  - (150, 200) is above a size 100 fish at (100, 400)
;  - (150, 500) is *not* above a size 100 fish at (100, 400)
;  - (250, 200) is *not* above a size 100 fish at (100, 400)
;  - (50, 200)  is *not* above a size 100 fish at (100, 400)
;
; Strategy: struct decomp (FishState)
(define (above-fs-fish? x y fs)
  (above-span? x y (fs-posn fs) (fish-size (fs-fish fs))))

(check-expect
 (above-fs-fish? 125 200
                 (make-fs A-GOLDFISH (make-posn 100 400) 9))
 #true)
(check-expect
 (above-fs-fish? 125 500
                 (make-fs A-GOLDFISH (make-posn 100 400) 9))
 #false)


; above-span? : Number Number Posn Number -> Boolean
; Determines whether position (x, y) lies above the horizontal line segment
; that starts at `left-end` that runs rightward for `width` pixels
;
; Examples:
;  - above span:
(check-expect (above-span? 200 200 (make-posn 150 250) 100)
              #true)
;  - right of span (almost above, but span isn’t long enough):
(check-expect (above-span? 200 200 (make-posn 150 250) 25)
              #false)
; - left of span:
(check-expect (above-span? 200 200 (make-posn 250 250) 100)
              #false)
; - below span:
(check-expect (above-span? 200 200 (make-posn 150 100) 100)
              #false)
;
; Strategy: domain knowledge (Euclidean geometry)
(define (above-span? x y left-end width)
  (and (<= (posn-x left-end) x (+ (posn-x left-end) width))
       (< y (posn-y left-end))))


;
;
;
;  ;;;;;;;;;
;      ;
;      ;
;      ;     ;        ;   ;;;;
;      ;     ;        ;  ;    ;
;      ;      ;      ;  ;      ;
;      ;      ;  ;;  ;  ;      ;
;      ;      ;  ;;  ;  ;      ;
;      ;      ;  ;;  ;  ;      ;
;      ;       ;;  ;;   ;      ;
;      ;       ;;  ;;    ;    ;
;      ;       ;;  ;;     ;;;;
;
; Two Fish in an Aquarium
;
;

;;;
;;; What if we get another fish?
;;;

; A DoubleAquarium is (make-aq2 FishState FishState)
; interp. if `aq` is a DoubleAquarium then all of:
;  - (aq2-fish1 aq) describes the first fish, and
;  - (aq2-fish2 aq) describes the second fish.
(define-struct aq2 [fish1 fish2])

; A SingleAquarium is a FishState

; process-double-aquarium : DoubleAquarium ... -> ...
; Template for DoubleAquarium.
#;
(define (process-double-aquarium aq ...)
  ... (aq2-fish1 aq) ...
  ... (aq2-fish2 aq) ...)


;;; Some helpful constants for making initial configurations:

(define AQ2-DX0/1    5)
(define AQ2-POSN0/1  (make-posn 0 (* 2/3 AQUARIUM-HEIGHT)))
(define AQ2-DX0/2    7)
(define AQ2-POSN0/2  (make-posn 0 (* 1/3 AQUARIUM-HEIGHT)))


; start2 : Fish Fish -> DoubleAquarium
; Big-bang runner for DoubleAquarium; takes two fish to
; start.
#;
(define (start2 fish1 fish2)
  (big-bang (make-aq2 (make-fs fish1 AQ2-POSN0/1 AQ2-DX0/1)
                      (make-fs fish2 AQ2-POSN0/2 AQ2-DX0/2))
    [to-draw  aq2-to-draw]
    [on-tick  aq2-on-tick]
    [on-mouse aq2-on-mouse]))


; aq2-to-draw : DoubleAquarium -> Image
; Renders both fish in the aquarium.
;
; Examples:
;  - (aq2-to-draw
;     (make-aq2 {goldfish at (100, 100), dx = 10}
;               {angelfish at (200, 200), dx = -8})
;    {scene with goldfish at (100, 100) facing right and angelfish at
;     (200, 200) facing left}
;
; Strategy: struct decomp
(define (aq2-to-draw aq)
  (place-fs (aq2-fish1 aq)
            (place-fs (aq2-fish2 aq)
                      AQUARIUM-BG)))

(check-expect
 (aq2-to-draw
  (make-aq2 (make-fs A-GOLDFISH (make-posn 100 100) 10)
            (make-fs AN-ANGELFISH (make-posn 200 200) -8)))
 (place-image/posn
  (fish->image A-GOLDFISH)
  (make-posn 100 100)
  (place-image/posn
   (flip-horizontal (fish->image AN-ANGELFISH))
   (make-posn 200 200)
   AQUARIUM-BG)))


; aq2-on-tick : DoubleAquarium -> DoubleAquarium
; Updates both fish positions and velocities over time.
;
; Examples:
;  - both fish moving right:
(check-expect
 (aq2-on-tick
  (make-aq2 (make-fs A-GOLDFISH (make-posn 100 100) 10)
            (make-fs AN-ANGELFISH (make-posn 200 200) 8)))
 (make-aq2 (make-fs A-GOLDFISH (make-posn 110 100) 10)
           (make-fs AN-ANGELFISH (make-posn 208 200) 8)))
;  - both fish turn around:
(check-expect
 (aq2-on-tick
  (make-aq2 (make-fs A-GOLDFISH (make-posn 5 100) -10)
            (make-fs AN-ANGELFISH (make-posn AQUARIUM-WIDTH 200) 7)))
 (make-aq2 (make-fs A-GOLDFISH (make-posn 15 100) 10)
           (make-fs AN-ANGELFISH (make-posn (- AQUARIUM-WIDTH 7) 200) -7)))
;
; Strategy: struct decomp
(define (aq2-on-tick aq)
  (make-aq2 (fs-on-tick (aq2-fish1 aq))
            (fs-on-tick (aq2-fish2 aq))))


; aq2-on-mouse : DoubleAquarium Nat Nat MouseEvent -> DoubleAquarium
; Handles mouse events to feed both fish.
;
; Examples:
; Assume fish #1 of size 100 at (100, 400), fish #2 of size 75 at
; (175, 200). Then:
;  - click at (150, 200) => fish #1 grows, fish #2 shrinks
;  - click at (187, 100) => both fishes grow
;  - click at (50, 200)  => neither fish grows
;
; Strategy: struct decomp
(define (aq2-on-mouse aq x y me)
  (make-aq2 (fs-on-mouse (aq2-fish1 aq) x y me)
            (fs-on-mouse (aq2-fish2 aq) x y me)))

(check-expect
  (aq2-on-mouse (make-aq2
                  (make-fs A-GOLDFISH (make-posn 100 400) 10)
                  (make-fs AN-ANGELFISH (make-posn 170 300) -20))
                 150 200 "button-down")
  (make-aq2 (make-fs (scale-fish GROW-RATE A-GOLDFISH)
                     (make-posn 100 400) 10)
            (make-fs (scale-fish SHRINK-RATE AN-ANGELFISH)
                     (make-posn 170 300) -20)))


;
;
;
;  ;;     ;;
;  ;;;   ;;;
;  ;;;   ;;;
;  ; ;; ;; ;    ;;;;;   ; ;;;;;   ;      ;
;  ; ;; ;; ;   ;    ;;  ;;    ;;   ;    ;
;  ;  ; ;  ;         ;  ;      ;   ;    ;
;  ;  ;;;  ;    ;;;;;;  ;      ;   ;;   ;
;  ;   ;   ;  ;;     ;  ;      ;    ;  ;
;  ;       ;  ;      ;  ;      ;    ;  ;
;  ;       ;  ;     ;;  ;      ;     ; ;
;  ;       ;  ;;   ;;;  ;      ;     ;;
;  ;       ;   ;;;;; ;  ;      ;     ;;
;                                     ;
;                                    ;
;                                  ;;
; As Many Fish as You’d Like in an Aquarium

;;;
;;; But how?
;;;

; An Aquarium/3 is (make-aq3 FishState FishState FishState)

; An Aquarium/4 is (make-aq4 FishState FishState FishState FishState)
#;
(define (aq4-on-tick aq)
  (make-aq4 (fs-on-tick (aq4-fish1 aq))
            (fs-on-tick (aq4-fish2 aq))
            (fs-on-tick (aq4-fish3 aq))
            (fs-on-tick (aq4-fish4 aq))))

; An Aquarium/4 is (make-aq4 DoubleAquarium DoubleAquarium)
#;
(define (aq4-on-tick aq)
  (make-aq4 (aq2-on-tick (aq4-sub1 aq)) (aq2-on-tick (aq4-sub2 aq))))

;; It gets pretty silly. The idea of nesting is correct, but we need a way
;; to make it uniform for all sizes.

;;;
;;; Which of these do you like better?
;;;

; An Aquarium is one of:
;  - (make-single-aquarium FishState)
;  - (make-extended-aquarium FishState Aquarium)
(define-struct single-aquarium [fish])
(define-struct extended-aquarium [fish extension])
; interp.
; if `aq` is an Aquarium then there are two possibilities:
;  - if (single-aquarium? aq) then `aq` contains just one fish,
;    (single-aquarium-fish aq); or
;  - if (extended-aquarium? aq) then `aq` contains both the single fish
;    (extended-aquarium-fish aq) and all the fish in the aquarium
;    (extended-aquarium-extension aq).

;;;
;;; Or:
;;;

; An Aquarium is one of:
;  - (make-empty-aquarium)
;  - (make-extended-aquarium FishState Aquarium)
(define-struct empty-aquarium [])
#;; reusing this from above:
(define-struct extended-aquarium [fish extension])
; interp.
; if `aq` is an Aquarium then there are two possibilities:
;  - if (empty-aquarium? aq) then `aq` contains no fish; or
;  - if (extended-aquarium? aq) then `aq` contains both the single fish
;    (extended-aquarium-fish aq) and all the fish in the (possibly empty)
;    aquarium (extended-aquarium-extension aq).


;; What differences do they have?
;;  - Do they both support the same range of numbers of fish?
;;  - How can we evaluate this question: “How will code using with them
;;    differ?” Do you have any tools to think about this without actually
;;    writing the code? (Yes: templates. But what does the template look
;;    like?)



;
;    ;;;;;;;      ;               ;            ;;
;    ;;           ;               ;            ;;
;    ;;                           ;            ;;
;    ;;        ;;;;       ;;;;;   ; ;;;;;      ;;
;    ;;           ;     ;;     ;  ;;    ;;     ;;
;    ;;;;;;;      ;     ;         ;      ;     ;;
;    ;;           ;     ;;        ;      ;     ;;
;    ;;           ;      ;;;;;    ;      ;     ;;
;    ;;           ;           ;;  ;      ;
;    ;;           ;            ;  ;      ;
;    ;;           ;     ;     ;;  ;      ;     ;;
;    ;;        ;;;;;;;   ;;;;;    ;      ;     ;;
;
;

;;
;; The rest of this file contains helper functions for rendering
;; fish. It's not especially interesting unless you're interested
;; in rendering fish.
;;

(define GOLDFISH-COLOR      "gold")
(define GOLDFISH-SCALE-X    2/3)
(define GOLDFISH-SCALE-Y    (- 1 GOLDFISH-SCALE-X))
(define ANGELFISH-SCALE     1/3)
(define ZEBRA-DANIO-SCALE-X 3/4)
(define ZEBRA-DANIO-SCALE-Y (- 1 ZEBRA-DANIO-SCALE-X))

; render-goldfish : Number Number Color -> Image
; Renders a goldfish with the given dimensions (approximately) and
; color.
;
; Examples:
;  - (render-goldfish 100 50 "blue") => an image of a blue goldfish with
;   100-by-50 pixel body (not including fins and tail).
;
; Strategy: domain knowledge
(define (render-goldfish width height color)
  (assemble-fish
   (ellipse width height "solid" color)
   (circle (* 1/10 height) "solid" "black")
   empty-image
   (rotate 15 (triangle (* 1/4 width) "solid" color))
   (rotate 30 (triangle height "solid" color))))

(check-expect (image? (render-goldfish 100 50 "gold")) #true)

; render-angelfish : Number Color -> Image
; Renders an angelfish with the given “radius” and color.
;
; Examples:
;  - (render-angelfish 75 "blue") => an image of a blue angelfish whose
;    body is a circle of radius 75.
;
; Strategy: domain knowledge
(define (render-angelfish radius color)
  (assemble-fish
   (circle radius "solid" color)
   (circle (* 1/20 radius) "solid" "black")
   (rotate 165 (isosceles-triangle (* 2 radius) 5 "solid" color))
   (rotate 15 (isosceles-triangle (* 3/2 radius) 5 "solid" color))
   (rotate 30 (triangle radius "solid" color))))

(check-expect (image? (render-angelfish 100 "gold")) #true)

; zebra-danio : Number Number ZebraDanioType Color Color -> Image
; Renders a zebra-danio with the given dimensions, type, body color, and
; stripe color.
;
; Examples:
;  - (render-zebra-danio 200 50 "triple" "blue" "white")
;    => an image of a blue zebra-danio fish with three white stripes whose
;    body is 200-by-50 pixels (not including fins and tail)
;
; Strategy: domain knowledge
(define (render-zebra-danio width height type bg-color fg-color)
  (assemble-fish
   (overlay
    (zebra-danio-stripes (* 1/2 width) (* 1/2 height) type fg-color)
    (ellipse width height "solid" bg-color))
   (overlay
    (circle (* 1/14 height) "solid" "black")
    (circle (* 1/10 height) "solid" "white"))
   (rotate 45 (triangle (* 1/5 width) "solid" bg-color))
   (rotate 15 (triangle (* 1/5 width) "solid" bg-color))
   (rotate 30 (triangle (* 1/2 height) "solid" bg-color))))

(check-expect (image? (render-zebra-danio 200 50 "triple" "blue" "white"))
              #true)

; zebra-danio-stripes : Number Number ZebraDanioType Color -> Image
; Renders the given type of stripes to an image of the given size in
; the given color.
;
; Examples:
;  - (zebra-danio-stripes 100 50 "double" "red")
;     => image of two horizontal red stripes in 100x50 field
;  - (zebra-danio-stripes 150 90 "triple" "green")
;     => image of three horizontal green stripes in 150x90 field
;
; Strategy: struct decomp
(define (zebra-danio-stripes width height type color)
  (cond
    [(string=? type "double")
     (double/above (horizontal-stripe width (* 1/2 height) "solid" color))]
    [(string=? type "triple")
     (triple/above (horizontal-stripe width (* 1/3 height) "solid" color))]
    [else
     (quadruple/above (horizontal-stripe width (* 1/4 height) "solid" color))]))

(check-expect (image? (zebra-danio-stripes 75 50 "double" "blue"))
              #true)
(check-expect (image? (zebra-danio-stripes 75 97 "triple" "blue"))
              #true)
(check-expect (image? (zebra-danio-stripes 200 100 "quadruple" "blue"))
              #true)


; double/above triple/above quadruple/above : Image -> Image
; Duplicates an image above itself {two, three, four} times.
;
; Strategy: function composition
(define (double/above i) (above i i))
(define (triple/above i) (above i i i))
(define (quadruple/above i) (above i i i i))

(check-expect (double/above TEST-IMAGE)
              (above TEST-IMAGE TEST-IMAGE))
(check-expect (triple/above TEST-IMAGE)
              (above TEST-IMAGE TEST-IMAGE TEST-IMAGE))
(check-expect (quadruple/above TEST-IMAGE)
              (above TEST-IMAGE TEST-IMAGE TEST-IMAGE TEST-IMAGE))


; horizontal-stripe : Number Number PenStyle Color -> Image
; Makes a horizontal stripe half of which is transparent vertical padding.
;
; Strategy: domain knowledge
(define (horizontal-stripe width height mode color)
  (overlay
   (line width
         0
         (pen color (round (* 1/2 height)) mode "round" "round"))
   (rectangle width height "solid" "transparent")))

(check-expect (image? (horizontal-stripe 50 10 "solid" "blue"))
              #true)


; assemble-fish : Image Image Image Image Image Image -> Image
; Assembles an image of a fish from images of its parts.
(define (assemble-fish body eye pectoral dorsal caudal)
  (underlay/align/offset
   "right" "bottom"
   pectoral
   (* 7/10 (image-width body)) (* -1/2 (image-height pectoral))
   (underlay/align/offset
    "right" "bottom"
    dorsal
    (* 7/10 (image-width body)) (* 3/4 (image-height body))
    (underlay/align/offset
     "left" "center"
     caudal
     (* 3/4 (image-width caudal)) 0
     (overlay/align/offset
      "right" "center"
      eye
      (* 1/10 (image-width body)) (* 1/12 (image-height body))
      body)))))

(check-expect (image? (assemble-fish empty-image
                                     empty-image
                                     empty-image
                                     empty-image
                                     empty-image))
              #true)
