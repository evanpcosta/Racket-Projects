;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ball-world) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; BOUNCING BALL BOX
;;;
;;; To start: Uncomment the `main` function right below this, and then
;;; call (main WORLD0).
;;;
;;; To play: press r and b to add balls.
;;;

; main : BallWorld -> BallWorld
; Calls big-bang to start the ball world.
#;
(define (main world0)
  (big-bang world0
    [to-draw render-ball-world]
    [on-tick handle-tick]
    [on-key  handle-keyboard]))


;   ;;;;;;;       ;;;     ;;         ;;        
;   ;;;;;;;;      ;;;     ;;         ;;        
;   ;;     ;;     ;;;     ;;         ;;        
;   ;;     ;;     ;;;     ;;         ;;        
;   ;;     ;;    ;; ;;    ;;         ;;        
;   ;;;;;;;;     ;; ;;    ;;         ;;        
;   ;;;;;;;;;    ;; ;;    ;;         ;;        
;   ;;     ;;;   ;; ;;    ;;         ;;        
;   ;;      ;;  ;;   ;;   ;;         ;;        
;   ;;      ;;  ;;;;;;;   ;;         ;;        
;   ;;      ;;  ;;;;;;;   ;;         ;;        
;   ;;     ;;;  ;;   ;;   ;;         ;;        
;   ;;;;;;;;;  ;;     ;;  ;;;;;;;;;  ;;;;;;;;; 
;   ;;;;;;;;   ;;     ;;  ;;;;;;;;;  ;;;;;;;;; 
;                                              
;                                                                    
;                                                                    
;             ;;       ;;   ;;;;;    ;;;;;;;    ;;         ;;;;;;    
;             ;;       ;;  ;;;;;;;   ;;;;;;;;   ;;         ;;;;;;;   
;             ;;       ;;  ;;   ;;   ;;    ;;;  ;;         ;;   ;;;  
;             ;;;     ;;; ;;     ;;  ;;     ;;  ;;         ;;    ;;  
;              ;; ;;; ;;  ;;     ;;  ;;     ;;  ;;         ;;     ;; 
;              ;; ;;; ;;  ;;     ;;  ;;    ;;;  ;;         ;;     ;; 
;              ;; ; ; ;;  ;;     ;;  ;;;;;;;;   ;;         ;;     ;; 
;              ;; ; ; ;;  ;;     ;;  ;;;;;;;    ;;         ;;     ;; 
;              ;; ; ; ;;  ;;     ;;  ;;   ;;;   ;;         ;;     ;; 
;              ;;;; ;;;;  ;;     ;;  ;;    ;;   ;;         ;;     ;; 
;              ;;;   ;;;  ;;     ;;  ;;     ;;  ;;         ;;    ;;  
;               ;;   ;;    ;;   ;;   ;;     ;;  ;;         ;;   ;;;  
;               ;;   ;;    ;;;;;;;   ;;      ;; ;;;;;;;;;  ;;;;;;;   
;               ;;   ;;     ;;;;;    ;;      ;;;;;;;;;;;;  ;;;;;;    
;                                                                    
;                                                                    
;                                                                    
;                                                                    

(require 2htdp/image)
(require 2htdp/universe)

; A Velocity is (make-vel Number Number)
; interp. x and y components of velocity
(define-struct vel (dx dy))

; A Ball is (make-ball Posn Velocity)
(define-struct ball (posn vel))

; A LoB is one of:
; -- empty
; -- (cons Ball LoB)

; A BallWorld is (make-ball-world LoB LoB)
; interp. one field contains red balls and the other blue
(define-struct ball-world (reds blues))


(define WIDTH 500)
(define HEIGHT 400)
(define RADIUS 10)
(define MIN-VEL 0)
(define MAX-VEL 10)

(define MT (empty-scene WIDTH HEIGHT))
(define RED-BALL (circle RADIUS "solid" "red"))
(define BLUE-BALL (circle RADIUS "solid" "blue"))

(define BALL-EX0 (make-ball (make-posn 10 20) (make-vel 3 -2)))
(define BALL-EX1 (make-ball (make-posn 100 10) (make-vel -10 10)))
(define BALL-EX2 (make-ball (make-posn 20 40) (make-vel 10 0)))
(define BALL-EX3 (make-ball (make-posn 100 300) (make-vel -10 10)))

(define WORLD0
  (make-ball-world (list BALL-EX0 BALL-EX1)
                   (list BALL-EX2 BALL-EX3)))


;                                                                    
;                                                                    
;                                                                    
;                                                                    
;   ;;;;;;;                                 ;;                       
;   ;;;;;;;;                                ;;                       
;   ;;    ;;;                               ;;                       
;   ;;     ;;                               ;;                       
;   ;;     ;;     ;;;;    ;;  ;;;      ;;;; ;;     ;;;;      ;;  ;;  
;   ;;    ;;;   ;;   ;;   ;;;;;;;;    ;;;;;;;;   ;;   ;;     ;; ;;;; 
;   ;;;;;;;;    ;;    ;;  ;;;   ;;;  ;;;   ;;;   ;;    ;;    ;;;   ; 
;   ;;;;;;;    ;;      ;  ;;     ;;  ;;     ;;  ;;      ;    ;;      
;   ;;   ;;;   ;;;;;;;;;  ;;     ;;  ;;     ;;  ;;;;;;;;;    ;;      
;   ;;    ;;   ;;         ;;     ;;  ;;     ;;  ;;           ;;      
;   ;;     ;;  ;;         ;;     ;;  ;;     ;;  ;;           ;;      
;   ;;     ;;   ;         ;;     ;;  ;;;   ;;;   ;           ;;      
;   ;;      ;;  ;;     ;  ;;     ;;   ;;;;;;;;   ;;     ;    ;;      
;   ;;      ;;;   ;;;;;;  ;;     ;;    ;;;; ;;     ;;;;;;    ;;      
;                                                                    
;                                                                    
;                                                                    
;                                                                    
;                                                                    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-ball-world : BallWorld -> Scene
; To draw the ball world
;
; Strategy: struct. decomp.
(define (render-ball-world bw)
  (render-balls
   (ball-world-reds bw)
   RED-BALL
   (render-balls
    (ball-world-blues bw)
    BLUE-BALL
    MT)))

(check-expect
  (render-ball-world
    (make-ball-world
      (list (make-ball (make-posn 50 70) (make-vel 5 5))
            (make-ball (make-posn 35 80) (make-vel 0 0)))
      (list (make-ball (make-posn 17 40) (make-vel 5 5))
            (make-ball (make-posn 75 20) (make-vel 0 0)))))
  (place-image/posn
    RED-BALL (make-posn 50 70)
    (place-image/posn
      RED-BALL (make-posn 35 80)
      (place-image/posn
        BLUE-BALL (make-posn 17 40)
        (place-image/posn
          BLUE-BALL (make-posn 75 20)
          MT)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render-balls : LoB Image Scene -> Scene
; To add the specified image at the specified locations.
;
; Strategy: struct. decomp.
(define (render-balls lob ball scene)
  (cond
    [(empty? lob) scene]
    [else         (place-ball
                   (first lob)
                   ball
                   (render-balls (rest lob) ball scene))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; place-ball : Ball Image Scene -> Scene
; To place the given ball (using the image) in the scene.
;
; Example:
(check-expect (place-ball (make-ball (make-posn 50 70) (make-vel 5 5))
                          BLUE-BALL MT)
              (place-image BLUE-BALL 50 70 MT))
;
; Strategy: struct. decomp.
(define (place-ball ball ball-img scene)
  (place-image/posn ball-img (ball-posn ball) scene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; place-image/posn : Image Posn Scene -> Scene
; To place the image at the given position
;
; Example:
(check-expect (place-image/posn RED-BALL (make-posn 50 70) MT)
              (place-image RED-BALL 50 70 MT))
;
; Strategy: struct. decomp.
(define (place-image/posn img posn scene)
  (place-image img (posn-x posn) (posn-y posn) scene))

;                                              
;                                              
;                                              
;                                              
;   ;;;;;;;;;;     ;;                 ;        
;   ;;;;;;;;;;     ;;                 ;        
;       ;;                            ;        
;       ;;                            ;        
;       ;;       ;;;;        ;;;;     ;    ;;; 
;       ;;         ;;      ;;;;;;;    ;   ;;;  
;       ;;         ;;      ;;    ;    ;  ;;;   
;       ;;         ;;     ;;          ; ;;;    
;       ;;         ;;     ;;          ;;;;     
;       ;;         ;;     ;;          ;;;;;    
;       ;;         ;;     ;;          ;  ;;    
;       ;;         ;;      ;;    ;    ;   ;;   
;       ;;         ;;      ;;;;;;;    ;   ;;;  
;       ;;      ;;;;;;;;     ;;;;     ;    ;;; 
;                                              
;                                              
;                                              
;                                              
;                                              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; handle-tick : BallWorld -> BallWorld
; To update the world at each tick
;
; Examples:
(check-expect (handle-tick
                (make-ball-world
                  (list (make-ball (make-posn 3 50) (make-vel -3 4)))
                  (list (make-ball (make-posn 40 80) (make-vel 5 7))
                        (make-ball (make-posn 40 82) (make-vel 1 2)))))
              (make-ball-world
                (list (make-ball (make-posn (* 2 RADIUS) 54) (make-vel 3 4)))
                (list (make-ball (make-posn 45 87) (make-vel 5 7))
                      (make-ball (make-posn 41 84) (make-vel 1 2)))))
;
(check-expect (handle-tick
                (make-ball-world
                  (list (make-ball (make-posn 3 50) (make-vel -3 4))
                        (make-ball (make-posn 40 82) (make-vel 1 2)))
                  (list (make-ball (make-posn 40 80) (make-vel 5 7)))))
              (make-ball-world
                (list (make-ball (make-posn (* 2 RADIUS) 54) (make-vel 3 4)))
                empty))
;
; Strategy: function composition
(define (handle-tick bw)
  (remove-collisions (update-positions bw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; update-positions : BallWorld -> BallWorld
; To move each ball by one time step.
;
; Example:
(check-expect (update-positions
                (make-ball-world
                  (list (make-ball (make-posn 3 50) (make-vel -3 4)))
                  (list (make-ball (make-posn 40 80) (make-vel 5 7)))))
              (make-ball-world
                (list (make-ball (make-posn (* 2 RADIUS) 54) (make-vel 3 4)))
                (list (make-ball (make-posn 45 87) (make-vel 5 7)))))
;
; Strategy: struct. decomp.
(define (update-positions bw)
  (make-ball-world
   (move-ball-list (ball-world-reds bw))
   (move-ball-list (ball-world-blues bw))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; move-ball-list : LoB -> LoB
; Move each ball in a list
;
; Example:
(check-expect (move-ball-list
                (list
                  (make-ball (make-posn 3 50) (make-vel -3 4))
                  (make-ball (make-posn 40 80) (make-vel 5 7))))
              (list
                (make-ball (make-posn (* 2 RADIUS) 54) (make-vel 3 4))
                (make-ball (make-posn 45 87) (make-vel 5 7))))
;
; Strategy: struct. decomp.
(define (move-ball-list lob)
  (cond
    [(empty? lob) empty]
    [else         (cons (move-ball (first lob))
                        (move-ball-list (rest lob)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; move-ball : Ball -> Ball
; To update the position of a ball by its velocity
;
; Example:
(check-expect (move-ball (make-ball (make-posn 40 80) (make-vel 5 7)))
              (make-ball (make-posn 45 87) (make-vel 5 7)))
;
; Strategy: struct. decomp.
(define (move-ball ball)
  (make-reflected-ball
   (update-posn (ball-posn ball) (ball-vel ball))
   (ball-vel ball)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; update-posn : Posn Velocity -> Posn
; To move a position by a velocity
;
; Example:
(check-expect (update-posn (make-posn 40 80) (make-vel 5 7))
              (make-posn 45 87))
;
; Strategy: struct. decomp.
(define (update-posn posn vel)
  (make-posn
   (+ (posn-x posn) (vel-dx vel))
   (+ (posn-y posn) (vel-dy vel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; make-reflected-ball : Posn Velocity -> Ball
; Constructs a ball with given parameters, reflecting if necessary
;
; Examples:
(check-expect (make-reflected-ball (make-posn 100 100)
                                   (make-vel 5 7))
              (make-ball (make-posn 100 100)
                         (make-vel 5 7)))
(check-expect (make-reflected-ball (make-posn 0 100)
                                   (make-vel -5 7))
              (make-ball (make-posn (* 2 RADIUS) 100)
                         (make-vel 5 7)))
;
; Strategy: struct. decomp.
(define (make-reflected-ball posn vel)
  (make-ball
   (make-posn
    (reflect-1d-posn RADIUS (posn-x posn) (- WIDTH RADIUS))
    (reflect-1d-posn RADIUS (posn-y posn) (- HEIGHT RADIUS)))
   (make-vel
    (reflect-1d-vel (vel-dx vel) RADIUS (posn-x posn) (- WIDTH RADIUS))
    (reflect-1d-vel (vel-dy vel) RADIUS (posn-y posn) (- HEIGHT RADIUS)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reflect-1d-posn : Number Number Number -> Number
; If coord isn't between lower and upper, reflect it so that it is.
;
; Examples:
(check-expect (reflect-1d-posn 5 35 50) 35)
(check-expect (reflect-1d-posn 5 55 50) 45)
(check-expect (reflect-1d-posn 5  2 50) 8)
;
; Strategy: decision tree
(define (reflect-1d-posn lower coord upper)
  (cond
    [(< coord lower) (- lower (- coord lower))]
    [(> coord upper) (- upper (- coord upper))]
    [else            coord]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reflect-1d-vel : Number Number Number Number -> Number
; If coord isn't between lower and upper, reflect the velocity
;
; Examples:
(check-expect (reflect-1d-vel 10 5 35 50) 10)
(check-expect (reflect-1d-vel 10 5 55 50) -10)
(check-expect (reflect-1d-vel 10 5  2 50) -10)
;
; Strategy: decision tree
(define (reflect-1d-vel vel lower coord upper)
  (cond
    [(< coord lower) (- vel)]
    [(> coord upper) (- vel)]
    [else            vel]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; remove-collisions : BallWorld -> BallWorld
; To remove any colliding balls from the world
;
; Example:
(check-expect
  (remove-collisions
    (make-ball-world (list BALL-EX0 BALL-EX1 BALL-EX2)
                     (list BALL-EX1 BALL-EX3)))
  (make-ball-world (list BALL-EX0 BALL-EX2)
                   (list BALL-EX3)))
;
; Strategy: struct. decomp.
(define (remove-collisions bw)
  (make-ball-world
   (remove-collisions/many (ball-world-blues bw) (ball-world-reds bw))
   (remove-collisions/many (ball-world-reds bw) (ball-world-blues bw))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; remove-collisions/many : LoB LoB -> LoB
; To remove balls from the second list that collide with the first
;
; Examples:
;  - (remove-collisions/many (list A C) (list A B C D)
;     => (list B D)
;
; Strategy: struct. decomp.
(define (remove-collisions/many which lob)
  (cond
    [(empty? which) lob]
    [else
     (remove-collisions/one
      (first which)
      (remove-collisions/many (rest which) lob))]))

(check-expect (remove-collisions/many
                (list BALL-EX0 BALL-EX2)
                (list BALL-EX0 BALL-EX1 BALL-EX2 BALL-EX3))
              (list BALL-EX1 BALL-EX3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; remove-collisions/one : Ball LoB -> LoB
; To remove all the balls that collide with the given ball
;
; Examples:
;  - (remove-collisions/one A (list B C)) where A touches B
;     => (list C)
;
; Strategy: struct. decomp.
(define (remove-collisions/one ball lob)
  (cond
    [(empty? lob) empty]
    [else         (cond
                    [(collide? ball (first lob))
                     (remove-collisions/one ball (rest lob))]
                    [else
                     (cons (first lob)
                           (remove-collisions/one ball (rest lob)))])]))

(check-expect (remove-collisions/one BALL-EX0
                                     (list BALL-EX1 BALL-EX0 BALL-EX2))
              (list BALL-EX1 BALL-EX2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; collide? : Ball Ball -> Boolean
; Are the balls touching.
;
; Examples:
; assumes RADIUS < 100
(check-expect (collide? (make-ball (make-posn 50 50) (make-vel 0 0))
                        (make-ball (make-posn 250 50) (make-vel 0 0)))
              false)
; assumes RADIUS > 3
(check-expect (collide? (make-ball (make-posn 50 50) (make-vel 0 0))
                        (make-ball (make-posn 55 50) (make-vel 0 0)))
              true)
;
; Strategy: domain knowledge
(define (collide? b1 b2)
  (< (distance (ball-posn b1) (ball-posn b2))
     (* 2 RADIUS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; distance : Posn Posn -> Number
; The distance between two positions.
;
; Examples:
(check-expect (distance (make-posn 0 0) (make-posn 3 4))   5)
(check-expect (distance (make-posn -1 -1) (make-posn 2 3)) 5)
;
; Strategy: struct. decomp.
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

;                                   
;                                   
;                                   
;                                   
;   ;;     ;;;                      
;   ;;    ;;;                       
;   ;;    ;;                        
;   ;;   ;;                         
;   ;;  ;;        ;;;;    ;;     ;; 
;   ;; ;;       ;;   ;;    ;;   ;;  
;   ;;;;;       ;;    ;;   ;;   ;;  
;   ;;;;;;     ;;      ;   ;;;  ;;  
;   ;;  ;;;    ;;;;;;;;;    ;; ;;   
;   ;;   ;;    ;;           ;; ;;   
;   ;;   ;;;   ;;            ;;;;   
;   ;;    ;;;   ;            ;;;    
;   ;;     ;;   ;;     ;      ;;    
;   ;;     ;;;    ;;;;;;     ;;;    
;                            ;;     
;                            ;;     
;                          ;;;;     
;                          ;;;      
;                                   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; handle-keyboard : BallWorld KeyEvt -> BallWorld
; Add random balls to the world at user request
;
; Examples:
;  - (handle-keyboard {a world with 3 red and 5 blue balls}) "r")
;      => {a world with 4 red and 5 blue balls}
;  - (handle-keyboard {a world with 3 red and 5 blue balls}) "b")
;      => {a world with 3 red and 6 blue balls}
;  - (handle-keyboard {a world with 3 red and 5 blue balls}) "a")
;      => {a world with 3 red and 5 blue balls}
;
; Strategy: struct. decomp.
(define (handle-keyboard bw key)
  (cond
    [(key=? key "r") (add-random-red bw)]
    [(key=? key "b") (add-random-blue bw)]
    [else            bw]))

(check-expect
  (length (ball-world-reds (handle-keyboard WORLD0 "r")))
  (+ 1 (length (ball-world-reds WORLD0))))
(check-expect
  (length (ball-world-blues (handle-keyboard WORLD0 "r")))
  (length (ball-world-blues WORLD0)))
(check-expect
  (length (ball-world-blues (handle-keyboard WORLD0 "b")))
  (+ 1 (length (ball-world-blues WORLD0))))
(check-expect
  (length (ball-world-reds (handle-keyboard WORLD0 "b")))
  (length (ball-world-reds WORLD0)))
(check-expect (handle-keyboard WORLD0 "a") WORLD0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; add-random-red : BallWorld -> BallWorld
; Adds a random red ball to the world.
;
; Example:
;  - (add-random-red {a world with 3 red and 5 blue balls})
;      => {a world with 4 red and 5 blue balls}
;
; Strategy: struct. decomp.
(define (add-random-red bw)
  (make-ball-world
   (cons (random-ball 0) (ball-world-reds bw))
   (ball-world-blues bw)))

(check-expect
  (length (ball-world-reds (add-random-red WORLD0)))
  (+ 1 (length (ball-world-reds WORLD0))))
(check-expect
  (length (ball-world-blues (add-random-red WORLD0)))
  (length (ball-world-blues WORLD0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; add-random-blue : BallWorld -> BallWorld
; Adds a random blue ball to the world.
;
; Example:
;  - (add-random-blue {a world with 3 red and 5 blue balls})
;      => {a world with 3 red and 6 blue balls}
;
; Strategy: struct. decomp.
(define (add-random-blue bw)
  (make-ball-world
   (ball-world-reds bw)
   (cons (random-ball 0) (ball-world-blues bw))))

(check-expect
  (length (ball-world-blues (add-random-blue WORLD0)))
  (+ 1 (length (ball-world-blues WORLD0))))
(check-expect
  (length (ball-world-reds (add-random-blue WORLD0)))
  (length (ball-world-reds WORLD0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; random-ball : Any -> Ball
; Chooses a randomly placed, randomly moving ball.
;
; Example:
;  - (random-ball 0) => a random ball
;
; Strategy: function composition
(define (random-ball dummy)
  (make-ball
   (make-posn (random-between 0 WIDTH) (random-between 0 HEIGHT))
   (make-vel (random-between MIN-VEL MAX-VEL)
             (random-between MIN-VEL MAX-VEL))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; random-between : Integer Integer -> Integer
; Chooses a random number in the interval [lower, upper].
;
; Example:
;  - (random-between 4 8) => 4, 5, 6, 7, or 8
;
; Strategy: function composition
(define (random-between lower upper)
  (+ lower (random (add1 (- upper lower)))))

(check-member-of (random-between 4 8) 4 5 6 7 8)
(check-member-of (random-between 4 8) 4 5 6 7 8)
(check-member-of (random-between 4 8) 4 5 6 7 8)
(check-member-of (random-between 4 8) 4 5 6 7 8)
(check-member-of (random-between 4 8) 4 5 6 7 8)
(check-member-of (random-between 4 8) 4 5 6 7 8)
(check-member-of (random-between 4 8) 4 5 6 7 8)


;                                                                    
;                                                                    
;                                                                    
;                                                                    
;   ;;;;;;                               ;;                          
;   ;;;;;;;                              ;;                          
;   ;;   ;;;                                                         
;   ;;    ;;                                                         
;   ;;     ;;     ;;;;      ;;;;;;     ;;;;       ;;;; ;;  ;;  ;;;   
;   ;;     ;;   ;;   ;;   ;;;;;;;;;      ;;      ;;;;;;;;  ;;;;;;;;  
;   ;;     ;;   ;;    ;;  ;;      ;      ;;     ;;;   ;;;  ;;;   ;;; 
;   ;;     ;;  ;;      ;  ;;;;;          ;;     ;;     ;;  ;;     ;; 
;   ;;     ;;  ;;;;;;;;;   ;;;;;;;       ;;     ;;     ;;  ;;     ;; 
;   ;;     ;;  ;;             ;;;;;      ;;     ;;     ;;  ;;     ;; 
;   ;;    ;;   ;;                ;;      ;;     ;;     ;;  ;;     ;; 
;   ;;   ;;;    ;         ;     ;;;      ;;     ;;;   ;;;  ;;     ;; 
;   ;;;;;;;     ;;     ;  ;;;;;;;;;      ;;      ;;;;;;;;  ;;     ;; 
;   ;;;;;;        ;;;;;;   ;;;;;;     ;;;;;;;;    ;;;; ;;  ;;     ;; 
;                                                      ;;            
;                                                ;    ;;;            
;                                                ;;;;;;;             
;                                                 ;;;;;              
;                         ;;;;;;                          
;                         ;;;;;;;                         
;                         ;;   ;;;                        
;                         ;;    ;;;    ;;;;;       ;;;;   
;                         ;;     ;;   ;;;;;;;    ;;;;;;;  
;                         ;;     ;;  ;;;   ;;;   ;;    ;  
;                         ;;     ;;  ;;     ;;  ;;        
;                         ;;     ;;  ;;     ;;  ;;        
;                         ;;     ;;  ;;     ;;  ;;        
;                         ;;    ;;;  ;;     ;;  ;;        
;                         ;;   ;;;   ;;;   ;;;   ;;    ;  
;                         ;;;;;;;     ;;;;;;;    ;;;;;;;  
;                         ;;;;;;       ;;;;;       ;;;;   
;                                                         
;                                                         


#|
DESIGN DOCUMENT: BOUNCING BALL BOX
Jesse Tov <jesse@cs>

High level description:

  Red and blue balls, with initially random position and velocity,
  bounce inside a box.  When a red ball collides with a blue ball, both
  balls disappear.  The user may add new balls, with random position
  and velocity, by pressing b for blue and r for red.  If a ball collides
  with more than one opposite-color ball at once, all balls involved in
  the collision are removed.

Parameters:

  - The width and height of the box
  - The radius of a ball
  - Range from which initial velocities are chosen
  - Numbers of initial balls

Data:

  At each point in time, there are a number of balls, each with a
  position and velocity.  This is all the information needed in our
  world.

  Two possible representation of the ball world:

  1)
     ;; A Velocity is (make-vel Number Number)
     (define-struct vel (dx dy))

     ;; A Ball is (make-ball Posn Velocity)
     (define-struct ball (posn vel))

     ;; A LoB is one of:
     ;; -- empty
     ;; -- (cons Ball LoB)

     ;; A BallWorld is (make-ball-world LoB LoB)
     ;; interp. one field contains red balls and the other blue
     (define-struct ball-world (reds blues))

  2)
     ;; A Velocity is (make-vel Number Number)
     (define-struct vel (dx dy))

     ;; A Ball is (make-ball Posn Velocity Color)
     (define-struct ball (posn vel color))

     ;; A LoB is one of:
     ;; -- empty
     ;; -- (cons Ball LoB)

     ;; A BallWorld is LoB

  Advantages of 1:
   - Possibly easier to find collisions.
  Advantages of 2:
   - Easier to extend with (many) more ball colors later.

  I intend to try 1, but if it gives me trouble, I might try 2.

Tasks:

 - Generate random balls

 - Update ball positions over time
    - update x and y by dx and dy

 - Detect collisions between balls and walls
    - for ball collisions, detect if the distance between the centers
      is less than the sum of the radii
    - for wall collisions, detect if the distance from the ball center
      to the wall is less than the radius; what about corners?

 - Figure out which way a ball should bounce
    - ?

 - Respond to user input

Possible factoring:

  render-ball-world : BallWorld -> Scene
  To render the ball world

  handle-keyboard : BallWorld KeyEvt -> BallWorld
  To add random balls to the world at user request

  handle-tick : BallWorld -> BallWorld
  To update to the next frame of the animation

  update-positions : BallWorld -> BallWorld
  To move each ball by its velocity

  remove-collisions : BallWorld -> BallWorld
  To remove balls that have collided from the ball world

  remove-collisions-many : LoB LoB -> LoB
  To remove all the balls in the first list that collide with
  a ball in the second list

  remove-collisions-one : LoB Ball -> LoB
  To remove all balls from the list that collide with the
  given ball

  collides? : Ball Ball -> Boolean
  Do the two balls collide?

  reflect-ball : Ball -> Ball
  Compute the bounce for one ball, if necessary

Questions:

 - What helper functions will reflect-ball require?
 - In what order should we move balls and check for collisions?
 - What should happen when all the balls run out?

Anticipated difficulties:

 - I really don't know how to do reflection yet.

Plan:

 1. Rendering first, because it might make it easier to
    understand the rest incrementally.

 2. Write some of the low level helpers, such as collide?,
    and test them.  (If I turn out not to need them as I
    anticipate, this could be a waste of time.)

 3. Figure out reflection.

 4. Put together the high level tick handler.

 5. Handle user interaction.

|#

