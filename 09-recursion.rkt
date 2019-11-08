;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 09-recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EECS 111 Lecture 9: Recursion
;;
;; Road map:
;;  1) Motivation: any number of fish
;;  2) Simpler example: sequences of strings
;;  3) Solution: any number of fish
;;  4) Extra example: trains

(require 2htdp/image)
(require 2htdp/universe)

;;;
;;; Which of these do you like better?
;;;

; An Aquarium is one of:
;  - (make-single-aquarium FishState)
;  - (make-extended-aquarium FishState Aquarium)
(define-struct single-aquarium [fish])
(define-struct extended-aquarium [fish extension])

;;;
;;; Or:
;;;

; An Aquarium is one of:
;  - (make-empty-aquarium)
;  - (make-extended-aquarium FishState Aquarium)
(define-struct empty-aquarium [])
#;; reusing this from above:
(define-struct extended-aquarium [fish extension])


;
;
;
;     ;;;;                            ;
;    ;   ;;     ;                     ;
;   ;           ;
;   ;         ;;;;;;      ; ;;;    ;;;;     ; ;;;;;     ;;;; ;    ;;;;;
;   ;           ;         ;;   ;      ;     ;;    ;;   ;    ;;  ;;     ;
;    ;;;        ;         ;           ;     ;      ;  ;      ;  ;
;      ;;;;     ;         ;           ;     ;      ;  ;      ;  ;;
;         ;;    ;         ;           ;     ;      ;  ;      ;   ;;;;;
;          ;    ;         ;           ;     ;      ;  ;      ;        ;;
;   ;      ;    ;         ;           ;     ;      ;  ;      ;         ;
;   ;;    ;     ;         ;           ;     ;      ;   ;    ;;  ;     ;;
;    ;;;;;       ;;;      ;        ;;;;;;;  ;      ;    ;;;; ;   ;;;;;
;                                                            ;
;                                                      ;    ;
;                                                       ;;;;
;


;;
;; EXPERIMENT TIME
;;


; A StrSeq* is one of:
;  - (make-empty-ss)
;  - (make-extended-ss String StrSeq*)
(define-struct empty-ss [])
(define-struct extended-ss [first rest])

; A StrSeq+ is one of:
;  - (make-singleton-ss String)
;  - (make-extended-ss String StrSeq*)
(define-struct singleton-ss [only])


; Examples of StrSeq*:
(define SS*/0 (make-empty-ss))
(define SS*/1 (make-extended-ss "(one)" SS*/0))
(define SS*/2 (make-extended-ss "(two)" SS*/1))
(define SS*/5
  (make-extended-ss
   "(five)"
   (make-extended-ss
    "(four)"
    (make-extended-ss
     "(three)"
     SS*/2))))

; Examples of StrSeq+:
(define SS+/1 (make-singleton-ss "(one)"))
(define SS+/2 (make-extended-ss "(two)" SS+/1))
(define SS+/5
  (make-extended-ss
   "(five)"
   (make-extended-ss
    "(four)"
    (make-extended-ss
     "(three)"
     SS+/2))))


; process-ss* : StrSeq* ... -> ...
#;
(define (process-ss* ss ...)
  (cond
    [(empty-ss? ss)
     ...]
    [else
     ... (extended-ss-first ss) ...
     ... (process-ss* (extended-ss-rest ss) ...) ...]))


; process-ss+ : StrSeq+ ... -> ...
#;
(define (process-ss+ ss ...)
  (cond
    [(singleton-ss? ss)
     ... (singleton-ss-only ss) ...]
    [else
     ... (extended-ss-first ss) ...
     ... (process-ss+ (extended-ss-rest ss) ...) ...]))


; ss*-concat : StrSeq* -> String
; Concatenates a sequence of strings into to a single string.
;
; Examples:
(check-expect (ss*-concat SS*/0) "")
(check-expect (ss*-concat SS*/2) "(two)(one)")
(check-expect (ss*-concat SS*/5) "(five)(four)(three)(two)(one)")
;
; Strategy: struct decomp (StrSeq*)
(define (ss*-concat ss)
  (cond
    [(empty-ss? ss) ""]
    [else
     (string-append (extended-ss-first ss)
                    (ss*-concat (extended-ss-rest ss)))]))


; ss+-concat : StrSeq+ -> String
; Concatenates a sequence of strings into to a single string.
;
; Examples:
(check-expect (ss+-concat SS+/1) "(one)")
(check-expect (ss+-concat SS+/2) "(two)(one)")
(check-expect (ss+-concat SS+/5) "(five)(four)(three)(two)(one)")
;
; Strategy: struct decomp (StrSeq*)
(define (ss+-concat ss)
  (cond
    [(singleton-ss? ss) (singleton-ss-only ss) ]
    [else
     (string-append (extended-ss-first ss)
                    (ss+-concat (extended-ss-rest ss)))]))


; ss*-contains-empty? : StrSeq* -> String
; Checks whether any of the strings in a sequence is empty.
;
; Examples:
;  - starts with "":
(check-expect (ss*-contains-empty?
               (make-extended-ss "" SS*/2))
              #true)
;  - "" is second element:
(check-expect (ss*-contains-empty?
               (make-extended-ss "keep going..."
                                 (make-extended-ss "" SS*/2)))
              #true)
;  - five elements but none empty:
(check-expect (ss*-contains-empty? SS*/5)
              #false)
;  - no elements means no empty elements (take that, Aristotle!):
(check-expect (ss*-contains-empty? (make-empty-ss))
              #false)
;
; Strategy: struct decomp (StrSeq*)
(define (ss*-contains-empty? ss)
  (cond
    [(empty-ss? ss)  #false]
    [else            (or (string=? "" (extended-ss-first ss))
                         (ss*-contains-empty? (extended-ss-rest ss)))]))


; ss+-contains-empty? : StrSeq+ -> String
; Checks whether any of the strings in a sequence is empty.
;
; Examples:
;  - starts with "":
(check-expect (ss+-contains-empty?
               (make-extended-ss "" SS+/2))
              #true)
;  - "" is second element:
(check-expect (ss+-contains-empty?
               (make-extended-ss "keep going..."
                                 (make-extended-ss "" SS+/2)))
              #true)
;  - five elements but none empty:
(check-expect (ss+-contains-empty? SS+/5)
              #false)
;
; Strategy: struct decomp (StrSeq+)
(define (ss+-contains-empty? ss)
  (cond
    [(singleton-ss? ss)  (string=? "" (singleton-ss-only ss))]
    [else                (or (string=? "" (extended-ss-first ss))
                             (ss+-contains-empty? (extended-ss-rest ss)))]))

;
;
;
;    ;;;;;;;      ;               ;
;    ;;           ;               ;
;    ;;                           ;
;    ;;        ;;;;       ;;;;;   ; ;;;;;
;    ;;           ;     ;;     ;  ;;    ;;
;    ;;;;;;;      ;     ;         ;      ;
;    ;;           ;     ;;        ;      ;
;    ;;           ;      ;;;;;    ;      ;
;    ;;           ;           ;;  ;      ;
;    ;;           ;            ;  ;      ;
;    ;;           ;     ;     ;;  ;      ;
;    ;;        ;;;;;;;   ;;;;;    ;      ;
;
;
;
;


;;;
;;; Okay, back to the fish.
;;;

; An Aquarium is (make-aq FishList MouseTracker)
(define-struct aq [fishes mouse])
; interp. if `aq` is a Aquarium then all of:
;  - (aq-fishes aq) is the list of fish in the aquarium; and
;  - (aq-mouse aq) keeps track of where the mouse is and how fast
;    it's going.
;
; A FishList is one of:
;  - (make-no-fish)
;  - (make-more-fish FishState FishList)
(define-struct no-fish [])
(define-struct more-fish [first rest])
;
; An FishState is (make-fs Fish Posn Number)
(define-struct fs [fish posn dx])
; interp. if `fs` is a FishState then all of:
;  - (fs-fish fs) is the fish in the configuration,
;  - (fs-posn fs) is the position of the fish, and
;  - (fs-dx fs) is the horizontal velocity of the fish.
;
; A Fish is one of:
;  - (make-goldfish Number)
;  - (make-angelfish Number Color)
;  - (make-zebra-danio Number ZebraDanioType Color Color)
(define-struct goldfish [size])
(define-struct angelfish [size color])
(define-struct zebra-danio [size type body stripe])
;
; A ZebraDanioType is one of:
;  - "double"
;  - "triple"
;  - "quadruple"
;
; A MouseTracker is (make-mouse-tracker Speedometer Speedometer)
(define-struct mouse-tracker [x y])
; interp. if `mt` is a MouseTracker then (mouse-tracker-x mt) tracks the
; mouse pointer’s x coordinate, and (mouse-tracker-y mt) tracks its
; y coordinate.
;
; A Speedometer is (make-speedometer Number Number Number)
(define-struct speedometer [old current speed])
; interp. `old` is the value at the previous tick, `current` is the most
; recently observed value, and `speed` is the estimate of the current speed
; (per tick).

;; Templates:

; process-aquarium : Aquarium ... -> ...
#;
(define (process-aquarium aq ...)
  ... (aq-fishes aq) ...
  ... (aq-mouse aq) ...)

; process-fish-list : FishList ... -> ...
#;
(define (process-fish-list fishes ...)
  (cond
    [(no-fish? fishes)
     ...]
    [else
     ... (more-fish-first fishes) ...
     ... (process-fish-list (more-fish-rest fishes) ...) ...]))

; process-fs : FishState ... -> ...
; Template for FishState.
#;
(define (process-fs fs ...)
  ... (fs-fish fs) ...
  ... (fs-posn fs) ...
  ... (fs-dx fs) ...)

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


;; Constants:

(define AQUARIUM-WIDTH    640)
(define AQUARIUM-HEIGHT   480)
(define AQUARIUM-COLOR    "darkblue")
(define AQUARIUM-MARGIN   30)
(define GROW-RATE         4/3)
(define SHRINK-RATE       (/ 1 GROW-RATE))
(define AQUARIUM-BG
  (empty-scene AQUARIUM-WIDTH AQUARIUM-HEIGHT AQUARIUM-COLOR))

(define FISH-STATE-DX0    5)
(define FISH-STATE-POSN0  (make-posn 0 (* 2/3 AQUARIUM-HEIGHT)))

(define SPEEDOMETER0      (make-speedometer 0 0 0))
(define MOUSE-TRACKER0    (make-mouse-tracker SPEEDOMETER0 SPEEDOMETER0))

;; Data examples:

(define A-GOLDFISH (make-goldfish 100))
(define AN-ANGELFISH (make-angelfish 150 "silver"))
(define A-ZEBRA-DANIO (make-zebra-danio 75 "triple" "green" "white"))

(define GOLDFISH-STATE (make-fs A-GOLDFISH (make-posn 100 200) -8))
(define ANGELFISH-STATE (make-fs AN-ANGELFISH (make-posn 150 230) 5))
(define ZEBRA-DANIO-STATE (make-fs A-ZEBRA-DANIO (make-posn 300 300) 15))


;; Data examples:

(define ONE-FISHES (make-more-fish GOLDFISH-STATE (make-no-fish)))
(define TWO-FISHES (make-more-fish ANGELFISH-STATE ONE-FISHES))
(define THREE-FISHES (make-more-fish ZEBRA-DANIO-STATE TWO-FISHES))

(define AQUARIUM-0 (make-aq (make-no-fish) MOUSE-TRACKER0))
(define AQUARIUM-1 (make-aq ONE-FISHES MOUSE-TRACKER0))
(define AQUARIUM-2 (make-aq TWO-FISHES MOUSE-TRACKER0))
(define AQUARIUM-3 (make-aq THREE-FISHES MOUSE-TRACKER0))


;; Function definitions:

; start : Fishes -> Aquarium
; Big-bang runner for Aquarium; takes initial list of fishes.
#;
(define (start fishes)
  (big-bang (make-aq fishes MOUSE-TRACKER0)
    [to-draw  aquarium-to-draw]
    [on-tick  aquarium-on-tick]
    [on-mouse aquarium-on-mouse]
    [on-key   aquarium-on-key]))


; aquarium-to-draw : Aquarium -> Image
; Renders all the fish in the aquarium.
;
; Examples:
;  - (make-aq (make-no-fish) {a mouse tracker})
;    => {empty aquarium scene}
;  - (make-aq (make-more-fish
;              {goldfish at (100, 200) dx = -8}
;              (make-more-fish
;               {zebra danio at (500, 100) dx = 12}
;               (make-no-fish)))
;             {a mouse tracker})
;    => {aquarium scene with goldfish at (100, 200) facing leftward
;                        and zebra danio at (500, 100) facing rightward}
;
; Strategy: struct decomp (Aquarium)
(define (aquarium-to-draw aq)
  (fishes-to-draw (aq-fishes aq)))

(check-expect (aquarium-to-draw
               (make-aq (make-no-fish) MOUSE-TRACKER-20x90-7))
              AQUARIUM-BG)
(check-expect (aquarium-to-draw
               (make-aq THREE-FISHES MOUSE-TRACKER-20x90-7))
              (fishes-to-draw THREE-FISHES))


; aquarium-on-tick : Aquarium -> Aquarium
; Moves all the fishes for the tick and updates the estimate of the
; velocity of the mouse.
;
; Examples:
(check-expect
 (aquarium-on-tick
  (make-aq (make-more-fish GOLDFISH-STATE
                           (make-more-fish ANGELFISH-STATE
                                           (make-no-fish)))
           (make-mouse-tracker
            (make-speedometer 315 305 2)
            (make-speedometer  90 90 -6))))
 (make-aq (make-more-fish (fs-on-tick GOLDFISH-STATE)
                          (make-more-fish (fs-on-tick ANGELFISH-STATE)
                                                      (make-no-fish)))
          (make-mouse-tracker
           (make-speedometer 305 305 -10)
           (make-speedometer 90 90 0))))
;
; Strategy: struct decomp (Aquarium)
(define (aquarium-on-tick aq)
  (make-aq (fishes-on-tick (aq-fishes aq))
           (mt-on-tick (aq-mouse aq))))


; aquarium-on-mouse : Aquarium Nat Nat MouseEvent -> Aquarium
; Updates the fish by attempting to feed them and records the position
; in the mouse tracker.
;
; Examples:
;  - (aquarium-on-mouse (make-aq {fish A at P, fish B at Q}
;                                {a mouse tracker})
;                       {x of P} {less than y of P}
;                       "button-up")
;    => (make-aq {fish A grown at P, fish B shrunk at Q}
;                {mouse tracker with mouse position updated})
;
; Strategy: struct decomp (Aquarium)
(define (aquarium-on-mouse aq x y me)
  (make-aq (fishes-on-mouse (aq-fishes aq) x y me)
           (mt-on-mouse (aq-mouse aq) x y me)))

(check-expect
 (aquarium-on-mouse (make-aq THREE-FISHES
                             (make-mouse-tracker
                              (make-speedometer 10 30 5)
                              (make-speedometer 20 40 -5)))
                    35 90 "button-up")
 (make-aq (fishes-on-mouse THREE-FISHES 35 90 "button-up")
          (make-mouse-tracker
           (make-speedometer 10 35 5)
           (make-speedometer 20 90 -5))))


; aquarium-on-key : Aquarium KeyEvent -> Aquarium
; Dispatches on keystrokes, adding fish on "1", "2", and "3".
;
; Examples:
;  - (aquarium-on-key (make-aq {some fishes}
;                              {mouse at (34, 190) moving left at -8})
;                     "2")
;    => (make-aq (make-more-fish {angelfish at (34, 190) moving left at -8}
;                                {some fishes})
;                {mouse unchanged})(
;
; Strategy: struct decomp (KeyEvent)
(define (aquarium-on-key aq ke)
  (cond
    [(key=? "1" ke) (add-new-fish A-GOLDFISH aq)]
    [(key=? "2" ke) (add-new-fish AN-ANGELFISH aq)]
    [(key=? "3" ke) (add-new-fish A-ZEBRA-DANIO aq)]
    [else           aq]))

(check-expect
 (aquarium-on-key (make-aq TWO-FISHES MOUSE-TRACKER-20x90-7) "1")
 (make-aq (make-more-fish (initialize-fs A-GOLDFISH MOUSE-TRACKER-20x90-7)
                          TWO-FISHES)
          MOUSE-TRACKER-20x90-7))
(check-expect
 (aquarium-on-key (make-aq TWO-FISHES MOUSE-TRACKER-20x90-7) "2")
 (make-aq (make-more-fish (initialize-fs AN-ANGELFISH MOUSE-TRACKER-20x90-7)
                          TWO-FISHES)
          MOUSE-TRACKER-20x90-7))
(check-expect
 (aquarium-on-key (make-aq TWO-FISHES MOUSE-TRACKER-20x90-7) "3")
 (make-aq (make-more-fish (initialize-fs A-ZEBRA-DANIO MOUSE-TRACKER-20x90-7)
                          TWO-FISHES)
          MOUSE-TRACKER-20x90-7))
(check-expect (aquarium-on-key AQUARIUM-2 "h") AQUARIUM-2)

; add-new-fish : Fish Aquarium -> Aquarium
; Adds the given fish to the given aquarium following the mouse.
;
; Examples:
;  - (add-new-fish
;     {angelfish}
;     (make-aq (make-no-fish)
;              {mouse is at (200, 100) moving right at 8 px/tick}))
;    => (make-aq
;        (make-more-fish {angelfish at (200, 100) moving right at 8 px/tick}
;                        (make-no-fish))
;        {mouse is same as input})
(check-expect
 (add-new-fish A-GOLDFISH (make-aq THREE-FISHES MOUSE-TRACKER-20x90-7))
 (make-aq (make-more-fish
           (make-fs A-GOLDFISH (make-posn 20 90) -7)
           THREE-FISHES)
          MOUSE-TRACKER-20x90-7))
;
; Strategy: struct decomp (Aquarium)
(define (add-new-fish fish aq)
  (make-aq (make-more-fish
            (initialize-fs fish (aq-mouse aq))
            (aq-fishes aq))
           (aq-mouse aq)))


; initialize-fs : Fish MouseTracker -> FishState
; Creates a fish configuration, determining the position and velocity
; from the mouse tracker.
;
; Examples:
;  - (initialize-fs {goldfish} {mouse at (60, 80) moving right at 5 px/tick})
;    => {goldfish at (60, 80) moving right at 5 px/tick}
;  - (initialize-fs {goldfish} {mouse at (60, 80) moving right at 5 px/tick})
;    => {goldfish at (60, 80) moving right at 5 px/tick}
;
; Strategy: function composition
(define (initialize-fs fish mt)
  (make-fs fish (mt-posn mt) (mt-dx mt)))

(check-expect (initialize-fs A-GOLDFISH MOUSE-TRACKER-20x90-7)
              (make-fs A-GOLDFISH (make-posn 20 90) -7))


; fishes-to-draw : Fishes -> Image
; Renders all the fish, with earlier fish overlayed later fish.
;
; Examples:
(check-expect (fishes-to-draw (make-no-fish))
              AQUARIUM-BG)
(check-expect (fishes-to-draw (make-more-fish GOLDFISH-STATE (make-no-fish)))
              (place-fs GOLDFISH-STATE AQUARIUM-BG))
(check-expect (fishes-to-draw (make-more-fish
                                ANGELFISH-STATE
                                (make-more-fish GOLDFISH-STATE (make-no-fish))))
              (place-fs ANGELFISH-STATE (place-fs GOLDFISH-STATE AQUARIUM-BG)))
;
; Strategy: struct decomp (Fishes)
(define (fishes-to-draw fishes)
  (cond
    [(no-fish? fishes)   AQUARIUM-BG]
    [else                (place-fs
                          (more-fish-first fishes)
                          (fishes-to-draw (more-fish-rest fishes)))]))


; fishes-on-tick : Fishes -> Fishes
; Updates each fish in response to a tick.
;
; Examples:
(check-expect (fishes-on-tick (make-no-fish))
              (make-no-fish))
(check-expect (fishes-on-tick (make-more-fish GOLDFISH-STATE (make-no-fish)))
              (make-more-fish (fs-on-tick GOLDFISH-STATE) (make-no-fish)))
(check-expect (fishes-on-tick (make-more-fish
                                ANGELFISH-STATE
                                (make-more-fish GOLDFISH-STATE (make-no-fish))))
              (make-more-fish
               (fs-on-tick ANGELFISH-STATE)
               (make-more-fish (fs-on-tick GOLDFISH-STATE) (make-no-fish))))
;
; Strategy: struct decomp (Fishes)
(define (fishes-on-tick fishes)
  (cond
    [(no-fish? fishes)   (make-no-fish)]
    [else                (make-more-fish
                          (fs-on-tick (more-fish-first fishes))
                          (fishes-on-tick (more-fish-rest fishes)))]))


; fishes-on-mouse : Fishes Nat Nat MouseEvent -> Fishes
; Updates each fish in response to the mouse.
;
; Examples:
(check-expect (fishes-on-mouse (make-no-fish) 200 100 "drag")
              (make-no-fish))
(check-expect (fishes-on-mouse (make-more-fish GOLDFISH-STATE (make-no-fish))
                               200 100 "drag")
              (make-more-fish (fs-on-mouse GOLDFISH-STATE 200 100 "drag")
                              (make-no-fish)))
(check-expect (fishes-on-mouse (make-more-fish
                                ANGELFISH-STATE
                                (make-more-fish GOLDFISH-STATE (make-no-fish)))
                               200 100 "drag")
              (make-more-fish
               (fs-on-mouse ANGELFISH-STATE 200 100 "drag")
               (make-more-fish (fs-on-mouse GOLDFISH-STATE 200 100 "drag")
                               (make-no-fish))))
;
; Strategy: struct decomp (Fishes)
(define (fishes-on-mouse fishes x y me)
  (cond
    [(no-fish? fishes)   (make-no-fish)]
    [else                (make-more-fish
                          (fs-on-mouse (more-fish-first fishes) x y me)
                          (fishes-on-mouse (more-fish-rest fishes) x y me))]))



;
;
;
;  ;;;;;;;;;                          ;
;      ;                              ;
;      ;
;      ;        ; ;;;     ;;;;;    ;;;;     ; ;;;;;     ;;;;;
;      ;        ;;   ;   ;    ;;      ;     ;;    ;;  ;;     ;
;      ;        ;              ;      ;     ;      ;  ;
;      ;        ;         ;;;;;;      ;     ;      ;  ;;
;      ;        ;       ;;     ;      ;     ;      ;   ;;;;;
;      ;        ;       ;      ;      ;     ;      ;        ;;
;      ;        ;       ;     ;;      ;     ;      ;         ;
;      ;        ;       ;;   ;;;      ;     ;      ;  ;     ;;
;      ;        ;        ;;;;; ;   ;;;;;;;  ;      ;   ;;;;;
;
;
;
;

;;
;; We start with our old definition of TrainCar; we'll define Train below.
;;

; A TrainCar is one of:
; -- (make-box-car Number Number Number)
; -- (make-hopper Number Number Number)
; -- (make-tank-car Number Number)
; -- (make-passenger-car Number)
; -- "engine"
(define-struct box-car       (length width height))
(define-struct hopper        (length width height))
(define-struct tank-car      (length radius))
(define-struct passenger-car (passengers))

; process-traincar : TrainCar ... -> ...
#;
(define (process-traincar tc ...)
  (cond
    [(box-car? tc)        ... (box-car-length tc) ...
                          ... (box-car-width tc) ...
                          ... (box-car-height tc) ...]
    [(hopper? tc)         ... (hopper-length tc) ...
                          ... (hopper-width tc) ...
                          ... (hopper-height tc) ...]
    [(tank-car? tc)       ... (tank-car-length tc) ...
                          ... (tank-car-radius tc) ...]
    [(passenger-car? tc)  ... (passenger-car-passengers tc) ...]
    [else                 ...]))

; Example TrainCars:
(define BOX-CAR-EX           (make-box-car 50 10 8))
(define HOPPER-EX            (make-hopper 50 10 8))
(define TANK-CAR-EX          (make-tank-car 50 5))
(define PASSENGER-CAR-EX     (make-passenger-car 80))


; train-car-volume : TrainCar -> Number
; Finds the volume of a train car
;
; Examples:
(check-expect (train-car-volume (make-box-car 3 4 5))    60)
(check-expect (train-car-volume (make-hopper 3 4 5))     30)
(check-within (train-car-volume (make-tank-car 3 4))     (* 3 pi (sqr 4)) 0.01)
(check-expect (train-car-volume (make-passenger-car 80)) 0)
(check-expect (train-car-volume "engine")                0)
;
; Strategy: struct. decomp. (TrainCar)
(define (train-car-volume tc)
  (cond
    [(box-car? tc)      (* (box-car-length tc)
                           (box-car-width tc)
                           (box-car-height tc))]
    [(hopper? tc)       (* 1/2
                           (hopper-length tc)
                           (hopper-width tc)
                           (hopper-height tc))]
    [(tank-car? tc)     (* (tank-car-length tc)
                           pi
                           (sqr (tank-car-radius tc)))]
    [(passenger-car? tc) 0]
    [else                0]))


;; Now we can define `Train`:

; A Train is one of:
; - (cons TrainCar Train)
; - '()


; process-train : Train ... -> ...
#;
(define (process-train train ...)
  (cond
    [(cons? train) ... (first train) ...
                   ... (process-train (rest train) ...) ...]
    [else          ...]))


; Example trains
(define TRAIN0-EX '())
(define TRAIN1-EX (cons
                   (make-box-car 50 10 8)
                   TRAIN0-EX))
(define TRAIN2-EX (cons
                   (make-hopper 50 10 8)
                   TRAIN1-EX))
(define TRAIN3-EX (cons
                   (make-box-car 50 10 12)
                   TRAIN2-EX))
(define TRAIN4-EX (cons
                   (make-tank-car 50 3)
                   TRAIN3-EX))


; train-volume : Train -> Number
; Adds of the freight volume of a train
;
; Examples:
;  - '() => 0
;  - box car (50 * 10 * 8), hopper (50 * 10 * 9), caboose => 6250
;
; Strategy: struct. decomp. (Train)
(define (train-volume train)
  (cond
    [(cons? train)  (+ (train-car-volume (first train))
                       (train-volume (rest train)))]
    [else           0]))

(check-expect (train-volume '()) 0)
(check-expect (train-volume (cons
                             (make-box-car 50 10 8)
                             (cons
                              (make-hopper 50 10 9)
                              '())))
              6250)


; train-has-engine? : Train -> Boolean
; Determines whether a train has an engine
;
; Examples:
;  - '() => false
;  - box car, tank car, caboose => false
;  - box car, engine, tank car, caboose => true
;
; Strategy: struct. decomp. (Train)
(define (train-has-engine? train)
  (cond
    [(cons? train)  (or (engine? (first train))
                        (train-has-engine? (rest train)))]
    [else           #false]))

(check-expect (train-has-engine? '())
              #false)

(check-expect (train-has-engine?
               (cons
                BOX-CAR-EX
                (cons
                 TANK-CAR-EX
                 '())))
              #false)

(check-expect (train-has-engine?
               (cons
                BOX-CAR-EX
                (cons
                 "engine"
                 (cons
                  TANK-CAR-EX
                  '()))))
              #true)


; engine? : TrainCar -> Boolean
; Is the given train car an engine?
;
(check-expect (engine? "engine") true)
(check-expect (engine? BOX-CAR-EX) false)
(check-expect (engine? HOPPER-EX) false)
(check-expect (engine? TANK-CAR-EX) false)
(check-expect (engine? PASSENGER-CAR-EX) false)
;
; Strategy: struct. decomp. (TrainCar)
(define (engine? tc)
  (cond
    [(box-car? tc)        #false]
    [(hopper? tc)         #false]
    [(tank-car? tc)       #false]
    [(passenger-car? tc)  #false]
    [else                 #true]))


; train-length : Train -> Nat
; Counts the number of cars in a train.
;
; Examples:
(check-expect (train-length '())
              1)
(check-expect (train-length
               (cons
                BOX-CAR-EX
                (cons
                 HOPPER-EX
                 (cons HOPPER-EX '()))))
              4)
;
; Strategy: struct. decomp. (Train)
(define (train-length train)
  (cond
    [(cons? train) (add1 (train-length (rest train)))]
    [else          1]))


; largest-car-volume : Train -> Number
; Finds the volume of the largest freight car in the train.
;
; Examples:
(check-expect (largest-car-volume '()) 0)
(check-expect (largest-car-volume
               (cons
                (make-box-car 10 10 20)
                (cons
                 (make-box-car 10 12 30)
                 (cons
                  (make-passenger-car 120)
                  '()))))
              3600)
;
; Strategy: struct. decomp. (Train)
(define (largest-car-volume train)
  (cond
    [(cons? train)
     (max (train-car-volume (first train))
          (largest-car-volume (rest train)))]
    [else 0]))



;
;
;
;    ;;;;;;;      ;               ;
;    ;;           ;               ;
;    ;;                           ;
;    ;;        ;;;;       ;;;;;   ; ;;;;;   ;      ;
;    ;;           ;     ;;     ;  ;;    ;;   ;    ;
;    ;;;;;;;      ;     ;         ;      ;   ;    ;
;    ;;           ;     ;;        ;      ;   ;;   ;
;    ;;           ;      ;;;;;    ;      ;    ;  ;
;    ;;           ;           ;;  ;      ;    ;  ;
;    ;;           ;            ;  ;      ;     ; ;
;    ;;           ;     ;     ;;  ;      ;     ;;
;    ;;        ;;;;;;;   ;;;;;    ;      ;     ;;
;                                               ;
;                                              ;
;                                            ;;
;
;
;   ;;    ;;             ;;;;
;   ;;    ;;                ;
;   ;;    ;;                ;
;   ;;    ;;    ;;;;        ;     ; ;;;;      ;;;;      ; ;;;     ;;;;;
;   ;;    ;;   ;;   ;       ;     ;;    ;    ;;   ;     ;;   ;  ;;     ;
;   ;;;;;;;;  ;;    ;;      ;     ;      ;  ;;    ;;    ;       ;
;   ;;    ;;  ;;     ;      ;     ;      ;  ;;     ;    ;       ;;
;   ;;    ;;  ;;;;;;;;      ;     ;      ;  ;;;;;;;;    ;        ;;;;;
;   ;;    ;;  ;;            ;     ;      ;  ;;          ;             ;;
;   ;;    ;;  ;;            ;     ;      ;  ;;          ;              ;
;   ;;    ;;   ;;    ;      ;     ;;    ;    ;;    ;    ;       ;     ;;
;   ;;    ;;    ;;;;;;       ;;;  ; ;;;;      ;;;;;;    ;        ;;;;;
;                                 ;
;                                 ;
;                                 ;
;


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



;; Where shall we keep our fish?



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


;;;
;;; Mouse tracker stuff
;;;

; mt-dx : MouseTracker -> Number
; Gets an estimate of the horizontal velocity of the mouse.
;
; Examples:
(check-expect (mt-dx (make-mouse-tracker
                      (make-speedometer 1 2 3)
                      (make-speedometer 4 5 6)))
              3)
;
; Strategy: struct decomp (MouseTracker & Speedometer)
(define (mt-dx mt)
  (speedometer-speed (mouse-tracker-x mt)))


; mt-posn : MouseTracker -> Posn
; Gets the current mouse position.
;
; Examples:
(check-expect (mt-posn
               (make-mouse-tracker
                      (make-speedometer 1 2 3)
                      (make-speedometer 4 5 6)))
              (make-posn 2 5))
;
; Strategy: struct decomp (MouseTracker & Speedometer)
(define (mt-posn mt)
  (make-posn (speedometer-current (mouse-tracker-x mt))
             (speedometer-current (mouse-tracker-y mt))))


; mt-on-mouse : MouseTracker Nat Nat MouseEvent -> MouseTracker
; Updates the state of the mouse with new information.
;
; Examples:
;
; Strategy: struct decomp (MouseTracker)
(define (mt-on-mouse mt x y me)
  (make-mouse-tracker (speedometer-on-change (mouse-tracker-x mt) x)
                      (speedometer-on-change (mouse-tracker-y mt) y)))


; mt-on-tick : MouseTracker -> MouseTracker
; Updates the mouse tracker for one timestep.
;
; Examples:
(check-expect (mt-on-tick (make-mouse-tracker
                           (make-speedometer 1 4 19)
                           (make-speedometer 12 25 -2)))
              (make-mouse-tracker
               (make-speedometer 4 4 3)
               (make-speedometer 25 25 13)))
;
; Strategy: struct decomp (MouseTracker)
(define (mt-on-tick mt)
  (make-mouse-tracker (speedometer-on-tick (mouse-tracker-x mt))
                      (speedometer-on-tick (mouse-tracker-y mt))))


; speedometer-on-change : Speedometer Number -> Speedometer
; Updates the speedometer with a new value.
;
; Examples:
(check-expect (speedometer-on-change (make-speedometer 5 10 300) -17)
              (make-speedometer 5 -17 300))
;
; Strategy: struct decomp (Speedometer)
(define (speedometer-on-change s current)
  (make-speedometer (speedometer-old s) current (speedometer-speed s)))


; speedometer-on-tick : Speedometer -> Speedometer
; Updates the speedometer to reflect time passing.
;
; Examples:
(check-expect (speedometer-on-tick (make-speedometer 5 12 300))
              (make-speedometer 12 12 7))
(check-expect (speedometer-on-tick (make-speedometer 5 2 300))
              (make-speedometer 2 2 -3))
;
; Strategy: struct decomp (Speedometer)
(define (speedometer-on-tick s)
  (make-speedometer (speedometer-current s)
                    (speedometer-current s)
                    (- (speedometer-current s) (speedometer-old s))))


(define MOUSE-TRACKER-20x90-7
  (make-mouse-tracker (make-speedometer 10 20 -7)
                      (make-speedometer 50 90 12)))


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
