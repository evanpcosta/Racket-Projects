;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 12-abstraction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lecture 12: Abstraction!
;;
;; Road map:
;;  1) A recipe for abstraction
;;  2) Higher-order list abstractions
;;  3) Abstraction in action


;
;
;
;   ;;;;;;                                              ;;;
;   ;     ;                         ;                 ;;;;;
;   ;      ;                        ;                 ;  ;;
;   ;      ;    ;;;;;     ; ;;;   ;;;;;;                 ;;
;   ;      ;   ;    ;;    ;;   ;    ;                    ;;
;   ;     ;          ;    ;         ;                    ;;
;   ;;;;;;      ;;;;;;    ;         ;                    ;;
;   ;         ;;     ;    ;         ;                    ;;
;   ;         ;      ;    ;         ;                    ;;
;   ;         ;     ;;    ;         ;                    ;;
;   ;         ;;   ;;;    ;         ;                    ;;
;   ;          ;;;;; ;    ;          ;;;               ;;;;;;
;
;

; An LoN is one of:
; - '()
; - (cons Number LoN)

;; What do these have in common?

; contains-4? : LoN -> Boolean
; Does the given list contain 4?
;
; Examples:
(check-expect (contains-4? '())         #false)
(check-expect (contains-4? '(1 4 30))   #true)
(check-expect (contains-4? '(1 0 30))   #false)
;
; Strategy: struct. decomp. (LoN)
(define (contains-4? lon)
  (cond
    [(empty? lon) #false]
    [else         (or (= (first lon) 4)
                      (contains-4? (rest lon)))]))


; contains-7? : LoN -> Boolean
; Does the given list contain 7?
;
; Examples:
(check-expect (contains-7? '())        #false)
(check-expect (contains-7? '(1 7 30))  #true)
(check-expect (contains-7? '(1 0 30))  #false)
;
; Strategy: struct. decomp. (LoN)
(define (contains-7? lon)
  (cond
    [(empty? lon) #false]
    [else         (or (= (first lon) 7)
                      (contains-7? (rest lon)))]))


; contains-17? : LoN -> Boolean
; Does the given list contain 17?
;
; Examples:
(check-expect (contains-17? '())         #false)
(check-expect (contains-17? '(1 17 30))  #true)
(check-expect (contains-17? '(1 0 30))   #false)
;
; Strategy: struct. decomp. (LoN)
(define (contains-17? lon)
  (cond
    [(empty? lon) #false]
    [else         (or (= (first lon) 17)
                      (contains-17? (rest lon)))]))


;; RECIPE FOR ABSTRACTION
;;
;; 1. Are the functions similar?  If not, don't do it.  If so,
;;    mark differences with difference lines.
;;
;; 2. Make a copy of one function, but with a new name.  Add a parameter
;;    for each difference line, add them to the recursive calls, and replace
;;    the difference line expressions.
;;
;; 3. Redefine original functions using the abstraction.
;;
;; 4. Redo the original tests using the abstraction.
;;
;; 5. Develop a generalized signature for the abstraction.
;;
;; 6. If possible, state the purpose using "...".


; contains-n? : Number LoN -> Boolean
; Does the given list contain n?
; (contains-n? n (list a_1 ... a_k)) = (or (= n a_1) ... (= n a_k))
;
; Examples:
(check-expect (contains-n? 4 '())        #false)
(check-expect (contains-n? 4 '(1 4 30))  #true)
(check-expect (contains-n? 4 '(1 0 30))  #false)
;
; Strategy: struct. decomp. (LoN)
(define (contains-n? n lon)
  (cond
    [(empty? lon) #false]
    [else         (or (= (first lon) n)
                      (contains-n? n (rest lon)))]))


; contains-4?^, contains-7?^, contains-17?^ : LoN -> Boolean
; Does the given list contain {4, 7, 17}?
;
; Strategy: function composition
(define (contains-4?^ lon)
  (contains-n? 4 lon))
(define (contains-7?^ lon)
  (contains-n? 7 lon))
(define (contains-17?^ lon)
  (contains-n? 17 lon))

(check-expect (contains-4?^ '())         #false)
(check-expect (contains-4?^ '(1 4 30))   #true)
(check-expect (contains-4?^ '(1 0 30))   #false)
(check-expect (contains-7?^ '())         #false)
(check-expect (contains-7?^ '(1 7 30))   #true)
(check-expect (contains-7?^ '(1 0 30))   #false)
(check-expect (contains-17?^ '())        #false)
(check-expect (contains-17?^ '(1 17 30)) #true)
(check-expect (contains-17?^ '(1 0 30))  #false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; An LoN is one of:
; - '()
; - (cons Number LoN)

; lon-length : LoN -> Natural
; Finds the length of a list of numbers.
;
; Examples:
(check-expect (lon-length '())      0)
(check-expect (lon-length '(1 2 3)) 3)
;
; Strategy: struct. decomp. (LoN)
(define (lon-length lon)
  (cond
    [(empty? lon) 0]
    [else         (add1 (lon-length (rest lon)))]))


; An LoS is one of:
; - '()
; - (cons String LoS)

; los-length : LoS -> Natural
; Finds the length of a list of strings.
;
; Examples:
(check-expect (los-length '())                    0)
(check-expect (los-length '("one" "two" "three")) 3)
;
; Strategy: struct. decomp. (LoS)
(define (los-length los)
  (cond
    [(empty? los) 0]
    [else         (add1 (los-length (rest los)))]))


; An LoI is one of:
; - '()
; - (cons Image LoI)

; loi-length : LoI -> Natural
; Finds the length of a list of images.
;
; Examples:
(check-expect (loi-length '())                        0)
(check-expect (loi-length (list (text "1" 12 "red")
                                (text "2" 12 "red")
                                (text "3" 12 "red"))) 3)
;
; Strategy: struct. decomp. (LoI)
(define (loi-length loi)
  (cond
    [(empty? loi) 0]
    [else         (add1 (loi-length (rest loi)))]))


; For any class of data ELEM, a [List-of ELEM] is one of:
;  - '()
;  - (cons ELEM [List-of ELEM])

; loe-length : [List-of ELEM] -> Natural
; Finds the length of a list.
;
; Strategy: struct. decomp. ([List-of ELEM])
(define (loe-length loe)
  (cond
    [(empty? loe) 0]
    [else         (add1 (loe-length (rest loe)))]))

(check-expect (loe-length '())                     0)
(check-expect (loe-length '(1 2 3))                3)
(check-expect (loe-length '("one" "two" "three"))  3)
(check-expect (loe-length `(,(text "1" 12 "red")
                            ,(text "2" 12 "red")
                            ,(text "3" 12 "red"))) 3)

;
;
;
;   ;;;;;;                                             ;;;;;
;   ;     ;                         ;                 ;;    ;
;   ;      ;                        ;                 ;      ;
;   ;      ;    ;;;;;     ; ;;;   ;;;;;;                     ;
;   ;      ;   ;    ;;    ;;   ;    ;                        ;
;   ;     ;          ;    ;         ;                       ;
;   ;;;;;;      ;;;;;;    ;         ;                      ;
;   ;         ;;     ;    ;         ;                     ;
;   ;         ;      ;    ;         ;                    ;
;   ;         ;     ;;    ;         ;                   ;
;   ;         ;;   ;;;    ;         ;                  ;;
;   ;          ;;;;; ;    ;          ;;;              ;;;;;;;;
;
;

;; Higher-order list abstractions


; filter<3 : [List-of Number] -> [List-of Number]
; Keeps the numbers less than 3.
;
; Examples:
(check-expect (filter<3 '()) '())
(check-expect (filter<3 '(1 3 5 2 4 6)) '(1 2))
;
; Strategy: struct. decomp.
(define (filter<3 lon)
  (cond
    [(empty? lon) '()]
    [else         (if (< (first lon) 3)
                      (cons (first lon)
                            (filter<3 (rest lon)))
                      (filter<3 (rest lon)))]))


; filter<6 : [List-of Number] -> [List-of Number]
; Keeps the numbers less than 6.
;
; Examples:
(check-expect (filter<6 '()) '())
(check-expect (filter<6 '(1 3 5 2 4 6)) '(1 3 5 2 4))
;
; Strategy: struct. decomp.
(define (filter<6 lon)
  (cond
    [(empty? lon) '()]
    [else         (if (< (first lon) 6)
                      (cons (first lon)
                            (filter<6 (rest lon)))
                      (filter<6 (rest lon)))]))


;; Abstraction motto: "Copy and paste makes waste."


; filter<n : Number [List-of Number] -> [List-of Number]
; Keeps the numbers less than n.
;
; Examples:
(check-expect (filter<n 3 '()) '())
(check-expect (filter<n 6 '()) '())
(check-expect (filter<n 3 '(1 3 5 2 4 6)) '(1 2))
(check-expect (filter<n 6 '(1 3 5 2 4 6)) '(1 3 5 2 4))
;
; Strategy: struct. decomp.
(define (filter<n n lon)
  (cond
    [(empty? lon) '()]
    [else         (if (< (first lon) n)
                      (cons (first lon)
                            (filter<n n (rest lon)))
                      (filter<n n (rest lon)))]))


; filter>=n : Number [List-of Number] -> [List-of Number]
; Keeps the numbers greater than or equal to n.
;
; Examples:
(check-expect (filter>=n 3 '()) '())
(check-expect (filter>=n 6 '()) '())
(check-expect (filter>=n 3 '(1 3 5 2 4 6)) '(3 5 4 6))
(check-expect (filter>=n 6 '(1 3 5 2 4 6)) '(6))
;
; Strategy: struct. decomp.
(define (filter>=n n lon)
  (cond
    [(empty? lon) '()]
    [else         (if (>= (first lon) n)
                      (cons (first lon)
                            (filter>=n n (rest lon)))
                      (filter>=n n (rest lon)))]))


; filter-compare-n : [Number Number -> Boolean] Number [List-of Number] ->
;                    [List-of Number]
; Keeps all the numbers for which the comparison to n returns true.
;
; Examples:
(check-expect (filter-compare-n < 3 '()) '())
(check-expect (filter-compare-n < 3 '(1 3 5 2 4 6)) '(1 2))
(check-expect (filter-compare-n < 6 '(1 3 5 2 4 6)) '(1 3 5 2 4))
(check-expect (filter-compare-n >= 3 '(1 3 5 2 4 6)) '(3 5 4 6))
(check-expect (filter-compare-n >= 6 '(1 3 5 2 4 6)) '(6))
;
; Strategy: struct. decomp.
(define (filter-compare-n compare n lon)
  (cond
    [(empty? lon) '()]
    [else         (if (compare (first lon) n)
                      (cons (first lon)
                            (filter-compare-n compare n (rest lon)))
                      (filter-compare-n compare n (rest lon)))]))


; filter-longer-than-4 : [List-of String] -> [List-of String]
; Keeps all the strings of length greater than 4.
;
; Examples:
(check-expect (filter-longer-than-4 '())
              '())
(check-expect (filter-longer-than-4 '("filter" "longer" "than" "4"))
              '("filter" "longer"))
;
; Strategy: struct. decomp.
(define (filter-longer-than-4 los)
  (cond
    [(empty? los) '()]
    [else         (if (> (string-length (first los)) 4)
                      (cons (first los)
                            (filter-longer-than-4 (rest los)))
                      (filter-longer-than-4 (rest los)))]))


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


; filter-pred : [X -> Boolean] [List-of X] -> [List-of X]
; Keeps all the elements satisfying the given predicate.
;
; Examples:
(check-expect (filter-pred even? '())                      '())
(check-expect (filter-pred even? '(1 2 3 4 5 6))           '(2 4 6))
(check-expect (filter-pred positive? '(0 1 -2 3 -4 5 -6))  '(1 3 5))
;
; Strategy: struct. decomp.
(define (filter-pred pred? lox)
  (cond
    [(empty? lox) '()]
    [else         (if (pred? (first lox))
                      (cons (first lox)
                            (filter-pred pred? (rest lox)))
                      (filter-pred pred? (rest lox)))]))


;; Now we need make sure we can define our old functions in terms of
;; our new abstraction:


; filter<3^ : [List-of Number] -> [List-of Number]
; Keeps the numbers less than 3.
;
; Examples:
(check-expect (filter<3^ '()) '())
(check-expect (filter<3^ '(1 3 5 2 4 6)) '(1 2))
;
; Strategy: function composition
(define (filter<3^ lon)
  (filter-pred less-than-3? lon))

; less-than-3? : Number -> Boolean
; Determines whether a number is less than 3.
(define (less-than-3? elem)
  (< elem 3))


; filter<n^ : Number [List-of Number] -> [List-of Number]
; Keeps the numbers less than n.
;
; Examples:
(check-expect (filter<n^ 3 '()) '())
(check-expect (filter<n^ 6 '()) '())
(check-expect (filter<n^ 3 '(1 3 5 2 4 6)) '(1 2))
(check-expect (filter<n^ 6 '(1 3 5 2 4 6)) '(1 3 5 2 4))
;
; Strategy: function composition
(define (filter<n^ n lon)
  (local
    [(define (less-than-n? elem)
       (< elem n))] ;; needs to see `n` from the arguments to filter<n^.
    (filter-pred less-than-n? lon)))


; filter-longer-than-n^ : Number [List-of String] -> [List-of String]
; Keeps all the strings of length greater than n.
;
; Examples:
(check-expect (filter-longer-than-n^ 4 '()) '())
(check-expect (filter-longer-than-n^ 4 '("filter" "longer" "than" "4"))
              '("filter" "longer"))
(check-expect (filter-longer-than-n^ 2 '("a" "bc" "def" "ghijk" "l" "mno"))
              '("def" "ghijk" "mno"))
;
; Strategy: function composition
(define (filter-longer-than-n^ n los)
  (local
    [(define (longer-than-n? s)
       (> (string-length s) n))]
    (filter-pred longer-than-n? los)))


; remove-finished-pstars^ : ListOfPStar -> ListOfPStar
; Removes all pstars whose ticks remaining is 0.
;
; Examples:
(check-expect (remove-finished-pstars^ (list PSTAR0-1)) (list PSTAR0-1))
(check-expect (remove-finished-pstars^ (list PSTAR0-2)) '())
;
; Strategy: function composition
(define (remove-finished-pstars^ pstars)
  (filter pstar-alive? pstars))


; compare-to : [X X -> Boolean] X -> [X -> Boolean]
; Creates a predicate to compare to a given value.
;
; Examples:
;
;  - These will work in ISL+λ but don’t work in ISL:
;      - ((compare-to < 10) 15) => #false
;      - ((compare-to < 10) 5)  => #true
;
;  - For now, you can work around the syntax limitation by giving the
;    function a name before applying it:
(check-expect (local [(define pred? (compare-to < 10))] (pred? 15))
              #false)
;
;  - Or maybe there’s something willing to apply it for you:
(check-satisfied 5 (compare-to < 10))
(check-satisfied 5 odd?)
(check-satisfied 6 even?)
;
;  - Using `filter`:
(check-expect (filter (compare-to < 3) '(1 3 5 2 4 6)) '(1 2))
(check-expect (filter (compare-to < 6) '(1 3 5 2 4 6)) '(1 3 5 2 4))
(check-expect (filter (compare-to >= 3) '(1 3 5 2 4 6)) '(3 5 4 6))
;
; Strategy: domain knowledge?
(define (compare-to compare x)
  (local [(define (pred? y) (compare y x))]
    pred?))


;; You don't need to write filter-pred, but you need to *be able to*
;; write filter-pred.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Other things that should annoy us:

; add-n-to-list : Number [List-of Number] -> [List-of Number]
; Adds n to every element of a list.
;
; Examples:
(check-expect (add-n-to-list 2 '())        '())
(check-expect (add-n-to-list 2 '(0 2 3 7)) '(2 4 5 9))
(check-expect (add-n-to-list 5 '(0 2 3 7)) '(5 7 8 12))
;
; Strategy: struct. decomp.
(define (add-n-to-list n lon)
  (cond
    [(empty? lon) '()]
    [else         (cons (+ n (first lon))
                        (add-n-to-list n (rest lon)))]))


; move-mortars : [List-of Mortar] -> [List-of Mortar]
; Removes mortars that have exploded and then moves those that remain.
;
; Examples:
(check-expect (move-mortars (list MORTAR1-1 MORTAR1-1))
              (list MORTAR1-2 MORTAR1-2))
(check-expect (move-mortars '()) '())
;
; Strategy: function composition
(define (move-mortars mortars)
  (cond
    [(empty? mortars) '()]
    [else             (cons (move-mortar (first mortars))
                            (move-mortars (rest mortars)))]))
  

; lon->los : [List-of Number] -> [List-of String]
; Converts every number in the list to a string
;
; Examples:
(check-expect (lon->los '())        '())
(check-expect (lon->los '(0 2 3 7)) '("0" "2" "3" "7"))
;
; Strategy: struct. decomp.
(define (lon->los lon)
  (cond
    [(empty? lon) '()]
    [else         (cons (number->string (first lon))
                        (lon->los (rest lon)))]))


; make-circles : Color [List-of Number] -> [List-of Image]
; Maps a list of radii to a list of solid circles of the given color.
;
; Examples:
(check-expect (make-circles "green" '())      '())
(check-expect (make-circles "green" '(5))     `(,(circle 5 "solid" "green")))
(check-expect (make-circles "red" '(1 2 3 7)) `(,(circle 1 "solid" "red")
                                                ,(circle 2 "solid" "red")
                                                ,(circle 3 "solid" "red")
                                                ,(circle 7 "solid" "red")))
;
; Strategy: struct. decomp.
(define (make-circles color lon)
  (cond
    [(empty? lon) '()]
    [else         (cons (circle (first lon) "solid" color)
                        (make-circles color (rest lon)))]))


; my-map : [α -> β] [List-of α] -> [List-of β]
; (my-map f (list x_1 ... x_k)) = (list (f x_1) ... (f x_k))
;
; Examples:
(check-expect (my-map add1 '()) '())
(check-expect (my-map add1 '(0 2 3 7)) '(1 3 4 8))
(check-expect (my-map number->string '(0 2 3 7)) '("0" "2" "3" "7"))
(check-expect (my-map move-mortar (list MORTAR1-1 MORTAR1-1))
              (list MORTAR1-2 MORTAR1-2))
;
; Strategy: struct. decomp.
(define (my-map f lox)
  (cond
    [(empty? lox) empty]
    [else         (cons (f (first lox))
                        (my-map f (rest lox)))]))

(check-expect (local
                [(define (make-red-circle r) (circle r "solid" "red"))]
                (my-map make-red-circle '(1 2 3 7)))
              (list (circle 1 "solid" "red")
                    (circle 2 "solid" "red")
                    (circle 3 "solid" "red")
                    (circle 7 "solid" "red")))

;; You don't need to write my-map, since map is built in.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sum : [List-of Number] -> Number
; Sums the numbers in a list.
;
; Examples:
(check-expect (sum '())      0)
(check-expect (sum '(3 4 5)) 12)
;
; Strategy: struct. decomp.
(define (sum lon)
  (cond
    [(empty? lon)  0]
    [else          (+ (first lon)
                      (sum (rest lon)))]))


; product : [List-of Number] -> Number
; Multiplies the numbers in a list.
;
; Examples:
(check-expect (product '())      1)
(check-expect (product '(3 4 5)) 60)
;
; Strategy: struct. decomp.
(define (product lon)
  (cond
    [(empty? lon)  1]
    [else          (* (first lon)
                      (product (rest lon)))]))


; concat : [List-of String] -> String
; Appends the strings in a list.
;
; Examples:
(check-expect (concat '())                     "")
(check-expect (concat '("Hello" ", w" "orld")) "Hello, world")
;
; Strategy: struct. decomp.
(define (concat los)
  (cond
    [(empty? los)  ""]
    [else          (string-append (first los)
                                  (concat (rest los)))]))


; fold-right : [ELEM RESULT -> RESULT] RESULT [List-of ELEM] -> RESULT
; (fold-right f b (list x_1 x_2 ... x_k))
;     = (f x_1 (f x_2 ... (f x_k b) ...))
; Combines the elements of a list using some operation.
;
; Examples:
(check-expect (fold-right + 0 '(3 4 5)) 12)
(check-expect (fold-right * 1 '(3 4 5)) 60)
(check-expect (fold-right string-append "" '("Hello" ", w" "orld"))
              "Hello, world")
;
; Strategy: struct. decomp.
(define (fold-right combine start lox)
  (cond
    [(empty? lox)  start]
    [else          (combine (first lox)
                            (fold-right combine start (rest lox)))]))


; superimpose : Scene [List-of Image] -> Scene
; Superimposes the images in a list onto a given scene.
;
; (superimpose scene (list i_1 i_2 ... i_k) =
;   (place-image i_1 0 0
;     (place-image i_2 0 0
;       ...
;         (place-image i_k 0 0 scene) ...))
;
; Strategy: function composition
(define (superimpose scene0 loi)
  (local [(define (place-at-origin image scene)
            (place-image image 0 0 scene))]
    (fold-right place-at-origin scene0 loi)))

(check-expect (superimpose (empty-scene 400 400)
                           (list (circle 200 "outline" "red")
                                 (circle 300 "outline" "green")))
              (place-image (circle 200 "outline" "red")
                           0 0
                           (place-image (circle 300 "outline" "green")
                                        0 0
                                        (empty-scene 400 400))))

;; fold-right is called foldr

;; Every function recursive on one list that you've written in this
;; class thus far can be written using foldr.



;
;
;
;   ;;;;;;                                             ;;;;;
;   ;     ;                         ;                 ;     ;
;   ;      ;                        ;                        ;
;   ;      ;    ;;;;;     ; ;;;   ;;;;;;                     ;
;   ;      ;   ;    ;;    ;;   ;    ;                       ;;
;   ;     ;          ;    ;         ;                   ;;;;
;   ;;;;;;      ;;;;;;    ;         ;                       ;
;   ;         ;;     ;    ;         ;                        ;
;   ;         ;      ;    ;         ;                        ;
;   ;         ;     ;;    ;         ;                        ;
;   ;         ;;   ;;;    ;         ;                 ;     ;
;   ;          ;;;;; ;    ;          ;;;               ;;;;;
;
;


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
(define-struct vel [dx dy])
; interp. the velocity of an object in pixels/tick

; A PosnVel is (make-posnvel Posn Vel)
(define-struct posnvel [posn vel])
; interp. the position and velocity of an object

; A PStar is (make-pstar PosnVel Nat Color)
(define-struct pstar [posnvel ticks color])
; interp. one of the colored rocket fragments after exploding, where
; `ticks` gives the number of ticks remaining until it burns out.

; A Mortar is (make-mortar PosnVel Nat [List-of Vel] Nat Color)
(define-struct mortar [posnvel ticks pstar-vels pstar-ticks pstar-color])
; interp. a rocket before exploding, where `ticks` gives the number
; of ticks remaining before it explodes, `pstar-color` is the color
; of all its stars, and `pstar-vels` gives the initial velocities of
; all its stars relative to the mortar's velocity at the moment of
; explosion.

; A Fireshow is (make-fs [List-of Mortar] [List-of PStar])
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
(define PSTAR-COLORS     '("white" "green" "yellow" "red" "blue"))

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


; step-mortars : [List-of Mortar] -> [List-of Mortar]
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
  (map move-mortar (filter mortar-alive? mortars)))


; step-mortar : Mortar -> Mortar
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


; step-pstars : [List-of PStar] -> [List-of PStar]
; Moves all the stars for a tick.
;
; Examples:
(check-expect (step-pstars (list PSTAR0-1 PSTAR1-1))
              (list PSTAR0-2 PSTAR1-2))
(check-expect (step-pstars (list PSTAR0-2 PSTAR2-1))
              (list PSTAR2-2))
(check-expect (step-pstars (list PSTAR0-2 PSTAR0-2))
              '())
;
; Strategy: function composition
(define (step-pstars stars)
  (map move-pstar (filter pstar-alive? stars)))


; step-pstar : PStar -> PStar
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


; explode-mortars : [List-of Mortar] -> [List-of PStar]
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
  (foldr append '()
         (map explode-mortar
              (filter mortar-exploded? mortars))))


; explode-mortar : Mortar -> [List-of PStar]
; Gets the stars from one exploding mortar.
;
; Examples:
(check-expect (explode-mortar MORTAR1-2)
              (list PSTAR1-1 PSTAR2-1))
;
; Strategy: struct decomp
(define (explode-mortar mortar)
  (local
    [; build-pstar : Vel -> PStar
     ; Builds a PStar with the given velocity, based on `mortar`.
     ; Strategy: struct decomp
     (define (build-pstar vel)
       (make-pstar (move-posnvel-by-vel (mortar-posnvel mortar) vel)
                   (mortar-pstar-ticks mortar)
                   (mortar-pstar-color mortar)))]
    (map build-pstar (mortar-pstar-vels mortar))))


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
  (make-posnvel (move-posn-by-vel (posnvel-posn posnvel) (posnvel-vel posnvel))
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


; move-posn-by-vel : Posn Vel -> Posn
; Adds the velocity to the position to get the new position.
;
; Examples:
(check-expect (move-posn-by-vel (make-posn 3 4) (make-vel 10 20))
              (make-posn 13 24))
;
; Strategy: struct decomp (Posn and Vel)
(define (move-posn-by-vel posn vel)
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
; Draws all the fireworks in the firework show.
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


; render-pstars : [List-of PStar] Scene -> Scene
; Renders all the stars in the list into the given scene.
;
; Examples:
(check-expect (render-pstars '() MT) MT)
(check-expect (render-pstars (list PSTAR1-1 PSTAR2-1) MT)
              (render-pstar PSTAR1-1
                            (render-pstar PSTAR2-1 MT)))
;
; Strategy: function composition
(define (render-pstars stars scene)
  (foldr render-pstar scene stars))


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


; render-mortars : [List-of Mortar] Scene -> Scene
; Renders all the mortars in the list into the given scene.
;
; Examples:
(check-expect (render-mortars '() MT) MT)
(check-expect (render-mortars (list MORTAR1-1 MORTAR1-2) MT)
              (render-mortar MORTAR1-1
                             (render-mortar MORTAR1-2 MT)))
;
; Strategy: function composition
(define (render-mortars mortars scene)
  (foldr render-mortar scene mortars))


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


; random-vel-list : Nat Interval -> [List-of Vel]
; Generates a list of `n` random velocities whose speed is in `interval`
;
; Examples:
;  - (random-vel-list 0 (make-interval 5 9)) => '()
;  - (random-vel-list 3 (make-interval 0 0))
;      => (list (make-vel 0 0) (make-vel 0 0) (make-vel 0 0))
;  - (random-vel-list 4 (make-interval 3 5)) =>
;      => a list of 4 random velocities whose speeds are all between 3 and 5
;
; Strategy: struct decomp (Nat)
(define (random-vel-list n interval)
  (local
    [(define (build-one _i)
       (polar-velocity (random-from-interval interval)
                       (random-radians 0)))]
    (build-list n build-one)))

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


; random-from-list : [List-of ELEM] -> ELEM
; Chooses a random element of a list.
;
; Examples:
(check-expect (random-from-list '(8)) 8)
(check-random (random-from-list '(8 9 10 11))
              (+ 8 (random 4)))
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

