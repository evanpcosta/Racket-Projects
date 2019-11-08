;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-more-recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EECS 111 Lecture 10: More Recursion
;;
;; Road map:
;;  1) Visualizing lists
;;  2) Generating lists
;;  3) Trees

(require 2htdp/image)


;
;
;
;   ;             ;
;   ;             ;                 ;
;   ;                               ;
;   ;          ;;;;       ;;;;;   ;;;;;;      ;;;;;
;   ;             ;     ;;     ;    ;       ;;     ;
;   ;             ;     ;           ;       ;
;   ;             ;     ;;          ;       ;;
;   ;             ;      ;;;;;      ;        ;;;;;
;   ;             ;           ;;    ;             ;;
;   ;             ;            ;    ;              ;
;   ;             ;     ;     ;;    ;       ;     ;;
;   ;;;;;;;;   ;;;;;;;   ;;;;;       ;;;     ;;;;;
;
;

; A ListOfNumber is one of:
;  - '()
;  - (cons Number ListOfNumber)


; lon->image : ListOfNumber -> Image
; Visualizes a list of numbers.
;
; Examples:
(check-expect (lon->image '()) nil-image)
(check-expect (lon->image (cons 5 '()))
              (cons-image (im 5) nil-image))
(check-expect (lon->image (cons 5 (cons 7 (cons 9 '()))))
              (cons-image (im 5)
                          (cons-image (im 7)
                                      (cons-image (im 9)
                                                  nil-image))))
;
; Strategy: struct decomp
(define (lon->image lon)
  (cond
    [(empty? lon)  nil-image]
    [else          (cons-image (im (first lon))
                               (lon->image (rest lon)))]))


; lon->circles : ListOfNumber Color -> Image
; Visualizes a list of numbers using circles.
;
; Examples:
(check-expect (lon->circles '() "pink") nil-image)
(check-expect (lon->circles (cons 5 '()) "pink")
              (cons-image (circle 5 "solid" "pink") nil-image))
(check-expect (lon->circles (cons 20 (cons 15 (cons 5 '()))) "pink")
              (cons-image (circle 20 "solid" "pink")
                          (cons-image (circle 15 "solid" "pink")
                                      (cons-image (circle 5 "solid" "pink")
                                                  nil-image))))
;
; Strategy: struct decomp
(define (lon->circles lon color)
  (cond
    [(empty? lon)  nil-image]
    [else          (cons-image (circle (first lon) "solid" color)
                               (lon->circles (rest lon) color))]))


;
;
;
;  ;;     ;;
;  ;;;    ;;              ;
;  ;;;    ;;              ;
;  ;; ;   ;;    ;;;;;   ;;;;;;      ;;;;;
;  ;; ;   ;;   ;    ;;    ;       ;;     ;
;  ;;  ;  ;;         ;    ;       ;
;  ;;  ;  ;;    ;;;;;;    ;       ;;
;  ;;   ; ;;  ;;     ;    ;        ;;;;;
;  ;;   ; ;;  ;      ;    ;             ;;
;  ;;    ;;;  ;     ;;    ;              ;
;  ;;    ;;;  ;;   ;;;    ;       ;     ;;
;  ;;     ;;   ;;;;; ;     ;;;     ;;;;;
;
;
;


; A Nesting is one of:
;  - (make-base)
;  - (make-around Nesting)
(define-struct base [])
(define-struct around [inner])

#;; process-nesting : Nesting ... -> ...
(define (process-nesting nest ...)
  (cond
    [(base? nest) ...]
    [else         ... (process-nesting (around-inner nest) ...) ...]))

;; Helper constructors:

(define Z (make-base))
(define (S n) (make-around n))


; nesting->image : Nesting -> Image
; Visualizes a nesting.
;
; Examples:
(check-expect (nesting->image Z) nil-image)
(check-expect (nesting->image (S Z)) (around-image nil-image))
(check-expect (nesting->image (S (S (S Z))))
              (around-image (around-image (around-image nil-image))))
;
; Strategy: struct decomp (Nesting)
(define (nesting->image nesting)
  (cond
    [(base? nesting)  nil-image]
    [else             (around-image (nesting->image (around-inner nesting)))]))


; nesting-append : Nesting Nesting -> Nesting
; Combines two nestings into a nesting whose layers are the sum of the two.
;
; Examples:
(check-expect (nesting-append Z Z)
              Z)
(check-expect (nesting-append (S (S Z)) Z)
              (S (S Z)))
(check-expect (nesting-append Z (S (S Z)))
              (S (S Z)))
(check-expect (nesting-append (S (S (S Z))) (S (S Z)))
              (S (S (S (S (S Z))))))
;
; Strategy: struct decomp (n1: Nesting)
(define (nesting-append n1 n2)
  (cond
    [(base? n1) n2]
    [else       (make-around (nesting-append (around-inner n1) n2))]))


; nesting-repeat-append : Nesting Nesting -> Nesting
; Combines two nestings by repeating one as many times as the other is
; nested.
;
; Examples:
(check-expect (nesting-repeat-append Z Z)
              Z)
(check-expect (nesting-repeat-append (S (S Z)) Z)
              Z)
(check-expect (nesting-repeat-append Z (S (S Z)))
              Z)
(check-expect (nesting-repeat-append (S (S (S Z))) (S (S Z)))
              (S (S (S (S (S (S Z)))))))
;
; Strategy: struct decomp (n1: Nesting)
(define (nesting-repeat-append n1 n2)
  (cond
    [(base? n1) (make-base)]
    [else       (nesting-append
                 n2
                 (nesting-repeat-append (around-inner n1) n2))]))


; A Nat is one of:
;  - 0
;  - (add1 Nat)

#;; process-nat : Nat ... -> ...
(define (process-nat n ...)
  (cond
    [(zero? n)  ...]
    [else       ... (process-nat (sub1 n) ...) ...]))

; nat->nesting : Nat -> Nesting
; Converts a natural number to the equivalent nesting.
;
; Examples:
(check-expect (nat->nesting 0) Z)
(check-expect (nat->nesting 3) (S (S (S Z))))
;
; Strategy: struct decomp (Nat)
(define (nat->nesting n)
  (cond
    [(zero? n)  (make-base)]
    [else       (make-around (nat->nesting (sub1 n)))]))


; nesting->nat : Nesting -> Nat
; Converts a nesting to the equivalent natural number.
;
; Examples:
(check-expect (nesting->nat Z) 0)
(check-expect (nesting->nat (S (S (S Z)))) 3)
;
; Strategy: struct decomp (Nat)
(define (nesting->nat nest)
  (cond
    [(base? nest) 0]
    [else         (add1 (nesting->nat (around-inner nest)))]))


;; nesting->nat and nat->nesting are inverses, so they round trip. Here
;; we convert two Nats to Nestings, multiply them, and then convert back:

(check-expect (nesting->nat (nesting-repeat-append
                             (nat->nesting 7)
                             (nat->nesting 12)))
              (* 7 12))


; reverse-iota : Natural -> ListOfNumber
; Produces a list that counts down from n to 1.
;
; Examples:
(check-expect (reverse-iota 0) '())
(check-expect (reverse-iota 1) (cons 1 '()))
(check-expect (reverse-iota 4) (cons 4 (cons 3 (cons 2 (cons 1 '())))))
;
; Strategy: struct decomp (Nat)
(define (reverse-iota n)
  (cond
    [(zero? n) '()]
    [else      (cons n (reverse-iota (sub1 n)))]))


; iota : Natural -> ListOfNumber
; Produces a list that counts up from 0 to (sub1 n)
;
; Examples:
(check-expect (iota 0) '())
(check-expect (iota 1) (cons 0 '()))
(check-expect (iota 4) (cons 0 (cons 1 (cons 2 (cons 3 '())))))
;
; Strategy: function composition
(define (iota n)
  (iota/helper n n))


; iota/helper : Natural Natural -> ListOfNumber
; Produces a list that counts up from (- n i) to (- n 1).
;
; Examples:
(check-expect (iota/helper 0 10) '())
(check-expect (iota/helper 1 10) (cons 9 '()))
(check-expect (iota/helper 4 10) (cons 6 (cons 7 (cons 8 (cons 9 '())))))
;
; Strategy: struct decomp (Nat)
(define (iota/helper i n)
  (cond
    [(zero? i) '()]
    [else      (cons (- n i) (iota/helper (sub1 i) n))]))


; factorial : Natural -> Natural
; Computes factorial.
;
; Examples:
(check-expect (factorial 0) 1)
(check-expect (factorial 5) 120)
;
; Strategy: struct decomp (Nat)
(define (factorial n)
  (cond
    [(zero? n)  1]
    [else       (* n (factorial (sub1 n)))]))


;
;
;
;  ;;;;;;;;;
;      ;
;      ;
;      ;        ; ;;;     ;;;;      ;;;;      ;;;;;
;      ;        ;;   ;   ;;   ;    ;;   ;   ;;     ;
;      ;        ;       ;;    ;;  ;;    ;;  ;
;      ;        ;       ;;     ;  ;;     ;  ;;
;      ;        ;       ;;;;;;;;  ;;;;;;;;   ;;;;;
;      ;        ;       ;;        ;;              ;;
;      ;        ;       ;;        ;;               ;
;      ;        ;        ;;    ;   ;;    ;  ;     ;;
;      ;        ;         ;;;;;;    ;;;;;;   ;;;;;
;
;
;


; An StrTree is one of:
;  - (make-leaf)
;  - (make-node String StrTree StrTree)
(define-struct leaf [])
(define-struct node [element left right])


#;; process-str-tree : StrTree ... -> ...
(define (process-str-tree t ...)
  (cond
    [(leaf? t)  ...]
    [else       ... (node-element t) ...
                ... (process-str-tree (node-left t) ...) ...
                ... (process-str-tree (node-right t) ...) ...]))


;; Helpers for building:
(define L         (make-leaf))
(define (N e l r) (make-node e l r))


;; Examples of StrTrees:

(define TREE-0 L)
(define TREE-1 (N "Andy" L L))
(define TREE-2
  (N "Carlos"
     (N "Aish"
        (N "Andy" L (N "Alexis" L L))
        L)
     (N "Jenny"
        (N "Dan" (N "Calypso" L L) (N "Emmy" L L))
        (N "Mario" L (N "Paulina" (N "Owen" L L) L)))))


; tree->image : StrTree -> Image
; Visualizes a string tree of numbers.
;
; Examples:
(check-expect (tree->image L) nil-image)
(check-expect (tree->image (N "A" (N "B" L L) (N "C" L L)))
              (tnode-image (im "A")
                           (tnode-image (im "B")
                                        nil-image
                                        nil-image)
                           (tnode-image (im "C")
                                        nil-image
                                        nil-image)))
;
; Strategy: struct decomp
(define (tree->image t)
  (cond
    [(leaf? t)  nil-image]
    [else       (tnode-image (im (node-element t))
                             (tree->image (node-left t))
                             (tree->image (node-right t)))]))
                             

; tree-member? : StrTree String -> Boolean?
; Determines whether `s` is an element of `t`.
;
; Examples:
(check-expect (tree-member? TREE-0 "anyone") #false)
(check-expect (tree-member? TREE-2 "Carlos") #true)
(check-expect (tree-member? TREE-2 "Emmy") #true)
(check-expect (tree-member? TREE-2 "Mario") #true)
(check-expect (tree-member? TREE-2 "Waldo") #false)
;
; Strategy: struct decomp (StrTree)
(define (tree-member? t s)
  (cond
    [(leaf? t)  #false]
    [else       (or (string=? s (node-element t))
                    (tree-member? (node-left t) s)
                    (tree-member? (node-right t) s))]))


;; Binary search trees (BSTs) are ordered:

; An StrBST is one of:
;  - (make-leaf)
;  - (make-node String StrTree StrTree)
; INVARIANT:
;  - If (make-node e l r) is a `StrBST` then all the elements
;    in `l` are less than (according to `string<?`) `e`, and all the
;    elements in `r` are greater than `e`.


;; Examples:

;; A good example:
(define BST-1 TREE-2)
;; A broken example:
(define BST-CDE* (N "C" (N "D" L L) (N "E" L L)))
;; Fixed
(define BST-CDE (N "D" (N "C" L L) (N "E" L L)))


; bst-member? : StrTree String -> Boolean?
; Determines whether `s` is an element of `t`.
;
; Examples:
(check-expect (bst-member? BST-CDE* "C") #true)
(check-expect (bst-member? BST-CDE* "D") #false)
(check-expect (bst-member? BST-CDE* "E") #true)
(check-expect (bst-member? BST-CDE "C")  #true)
(check-expect (bst-member? BST-CDE "D")  #true)
(check-expect (bst-member? BST-CDE "E")  #true)
;
; Strategy: struct decomp (StrBST)
(define (bst-member? t s)
  (cond
    [(leaf? t)  #false]
    [else
     (cond
       [(string<? s (node-element t))
        (bst-member? (node-left t) s)]
       [(string>? s (node-element t))
        (bst-member? (node-right t) s)]
       [else    #true])]))
     
(check-expect (bst-member? L     "anyone") #false)
(check-expect (bst-member? BST-1 "Carlos") #true)
(check-expect (bst-member? BST-1 "Emmy") #true)
(check-expect (bst-member? BST-1 "Mario") #true)
(check-expect (bst-member? BST-1 "Waldo") #false)


;; We can convert between data structures:

; A ListOfString is one of:
;  - '()
;  - (cons String ListOfString)


; flatten : StrTree -> ListOfString
; Puts all the elements of a tree in order in a list.
;
; Examples:
(check-expect (flatten L) '())
(check-expect (flatten (N "A"
                          (N "B"
                             L
                             (N "C" L L))
                          (N "D"
                             (N "E" L L)
                             (N "F" L L))))
              (cons "B"
                    (cons "C"
                          (cons "A"
                                (cons "E"
                                      (cons "D"
                                            (cons "F" '())))))))
;
; Strategy: struct decomp (StrTree)
(define (flatten t)
  (cond
    [(leaf? t) '()]
    [else      (append (flatten (node-left t))
                       (cons (node-element t)
                             (flatten (node-right t))))]))


;
;
;
;   ;;;;;;                               ;
;   ;     ;                              ;
;   ;      ;                             ;
;   ;      ;    ;;;;    ; ;;;;;     ;;;; ;    ;;;;      ; ;;;
;   ;      ;   ;;   ;   ;;    ;;   ;    ;;   ;;   ;     ;;   ;
;   ;     ;;  ;;    ;;  ;      ;  ;      ;  ;;    ;;    ;
;   ;;;;;;    ;;     ;  ;      ;  ;      ;  ;;     ;    ;
;   ;     ;   ;;;;;;;;  ;      ;  ;      ;  ;;;;;;;;    ;
;   ;      ;  ;;        ;      ;  ;      ;  ;;          ;
;   ;      ;  ;;        ;      ;  ;      ;  ;;          ;
;   ;      ;   ;;    ;  ;      ;   ;    ;;   ;;    ;    ;
;   ;       ;   ;;;;;;  ;      ;    ;;;; ;    ;;;;;;    ;
;
;

;; Rendering constants

(define SIZE       15)
(define TEXT-COLOR "Black")
(define THICKNESS  2)
(define PADDING    8)
(define BG-COLOR   "LightYellow")
(define EMPTY-PEN  (pen "Black" THICKNESS "solid" "butt" "miter"))
(define CONS-PEN   (pen "DarkRed" THICKNESS "solid" "butt" "miter"))
(define TNODE-PEN  (pen "DarkBlue" THICKNESS "solid" "butt" "miter"))
(define SUCC-PEN   (pen "DarkGreen" THICKNESS "solid" "butt" "miter"))

; nil-image : Image
; Visualizes the empty list
(define nil-image
  (overlay (line SIZE (- SIZE) EMPTY-PEN)
           (circle (/ SIZE 2) "outline" EMPTY-PEN)))


;; Rendering functions

; cons-image : Image Image -> Image
; Renders a visualization of a cons cell.
;
; Examples:
;  - (cons-image {a circle} {a square})
;     => side-by-side boxes containing {a circle} and {a square}
;
; Strategy: function composition
(define (cons-image f r)
  (cons-image/helper f r
                     (max (image-height f) (image-height r))))

(check-expect (cons-image (rectangle 10 20 "solid" "blue")
                          (rectangle 15 10 "solid" "green"))
              (beside
               (overlay
                (rectangle (+ 10 PADDING) (+ 20 PADDING) "outline" CONS-PEN)
                (rectangle 10 20 "solid" "blue")
                (rectangle (+ 10 PADDING) (+ 20 PADDING) "solid" BG-COLOR))
               (overlay
                (rectangle (+ 15 PADDING) (+ 20 PADDING) "outline" CONS-PEN)
                (rectangle 15 10 "solid" "green")
                (rectangle (+ 15 PADDING) (+ 20 PADDING) "solid" BG-COLOR))))


; cons-image/helper : Image Image Number  -> Image
; Creates an image of a cons cell containing f and r, with h giving the
; height.
;
; Examples:
(check-expect (cons-image/helper (im 5) (im 6) 25)
              (beside (box (im 5) (image-width (im 5)) 25 CONS-PEN)
                      (box (im 6) (image-width (im 6)) 25 CONS-PEN)))
;
; Strategy: function composition
(define (cons-image/helper f r h)
  (beside (box f (image-width f) h CONS-PEN)
          (box r (image-width r) h CONS-PEN)))


; tnode-image : Image Image Image -> Image
; Renders a visualization of a tree node with element e, left child l,
; and right child r.
;
; Examples:
;  - (tnode-image {circle} {square} {triangle})
;    => a box divided horizontally, with the {circle} in the top portion,
;       and the bottom portion divided vertically between {square} and
;       {triangle}
;
; Strategy: function composition
(define (tnode-image e l r)
  (tnode-image/helper e l r
                      (max (+ PADDING (image-width e))
                           (+ (image-width l) (image-width r)))
                      (max (image-height l) (image-height r))))

(check-expect
 (tnode-image (circle 10 "solid" "green")
              (circle 20 "solid" "red")
              (circle 25 "solid" "blue"))
 (above
  (overlay
   (rectangle (+ (* 2 PADDING) 90) (+ PADDING 20) "outline" TNODE-PEN)
   (circle 10 "solid" "green")
   (rectangle (+ (* 2 PADDING) 90) (+ PADDING 20) "solid" BG-COLOR))
  (beside
   (overlay
    (rectangle (+ PADDING 40) (+ PADDING 50) "outline" TNODE-PEN)
    (circle 20 "solid" "red")
    (rectangle (+ PADDING 40) (+ PADDING 50) "solid" BG-COLOR))
   (overlay
    (rectangle (+ PADDING 50) (+ PADDING 50) "outline" TNODE-PEN)
    (circle 25 "solid" "blue")
    (rectangle (+ PADDING 50) (+ PADDING 50) "solid" BG-COLOR)))))


; tnode-image/helper : Image Image Image Number Numer -> Image
; Renders a tree node, with `w` the total width and `h` the height of the
; children.
;
; Examples:
(check-expect
 (tnode-image/helper (im 5) (im 6) (im 7) 100 75)
 (above (box (im 5) (+ PADDING 100) (image-height (im 5)) TNODE-PEN)
        (beside (box (im 6) 50 75 TNODE-PEN)
                (box (im 7) 50 75 TNODE-PEN))))
;
; Strategy: function composition
(define (tnode-image/helper e l r w h)
  (above (box e (+ PADDING w) (image-height e) TNODE-PEN)
         (beside/stretch/box l r (image-width l) (image-width r) w h)))


; beside/stretch/box : Image Image Number Numer Number Number -> Image
; Boxes l and r and places them beside each other, scaling their widths
; (lw and rw) to total w.
;
; Examples:
;  - Perfect fit:
(check-expect
 (beside/stretch/box (rectangle 20 10 "solid" "green")
                     (rectangle 10 15 "solid" "blue")
                     20 10 30 40)
 (beside (box (rectangle 20 10 "solid" "green") 20 40 TNODE-PEN)
         (box (rectangle 10 15 "solid" "blue") 10 40 TNODE-PEN)))
;  - Stretches 2 times:
(check-expect
 (beside/stretch/box (rectangle 20 10 "solid" "green")
                     (rectangle 10 15 "solid" "blue")
                     20 10 60 40)
 (beside (box (rectangle 20 10 "solid" "green") 40 40 TNODE-PEN)
         (box (rectangle 10 15 "solid" "blue") 20 40 TNODE-PEN)))
;
; Strategy: function composition
(define (beside/stretch/box l r lw rw w h)
  (beside (box l (* w (/ lw (+ lw rw))) h TNODE-PEN)
          (box r (* w (/ rw (+ lw rw))) h TNODE-PEN)))


; around-image : Image -> Image
; Visualizes the success by placing it in a box.
;
; Examples:
(check-expect (around-image (im "hi"))
              (box (im "hi")
                   (image-width (im "hi"))
                   (image-height (im "hi"))
                   SUCC-PEN))
;
; Strategy: function composition.
(define (around-image pred)
  (box pred (image-width pred) (image-height pred) SUCC-PEN))


; box : Image Number Number Pen -> Image
; Places `i` in a `w`-by-`h` PLUS PADDING box, drawn with `p`.
;
; Examples:
;  - (box {a circle} 40 50 {a green pen})
;     => {a circle} in a 40-by-50 green-outlined box
;
; Strategy: domain knowledge
(define (box i w h p)
  (overlay (rectangle (+ PADDING w) (+ PADDING h) "outline" p)
           i
           (rectangle (+ PADDING w) (+ PADDING h) "solid" BG-COLOR)))

(check-expect (box (im "boo!") 20 50 CONS-PEN)
              (overlay
               (rectangle (+ PADDING 20) (+ PADDING 50) "outline" CONS-PEN)
               (im "boo!")
               (rectangle (+ PADDING 20) (+ PADDING 50) "solid" BG-COLOR)))


; A Renderable is one of:
;  - Image
;  - String
;  - Number
; Interp. types we can easily render

; im : Renderable -> Image
; Turns any Renderable into an image.
;
; Examples:
(check-expect (im nil-image) nil-image)
(check-expect (im "hello") (text "hello" SIZE TEXT-COLOR))
(check-expect (im 88) (text "88" SIZE TEXT-COLOR))
;
; Strategy: struct decomp
(define (im r)
  (cond
    [(image? r) r]
    [(string? r) (text r SIZE TEXT-COLOR)]
    [(number? r) (text (number->string r) SIZE TEXT-COLOR)]))
