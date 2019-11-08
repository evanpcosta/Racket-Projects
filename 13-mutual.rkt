;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 13-mutual) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lecture 13: Mutual recursion
;;
;; Road map:
;;  1) Motivating problems
;;  2) Abstract ex{ampl,ercis}es
;;  3) Additional applications


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


#|

How would we represent these things?:

 - A descendant tree, where each node represents an alpaca with
any number of offspring. (AlpacaTree was an ancestor tree.)

 - A richer descendant tree where we associate some additional
information with each child, such as the identity of the other
parent.

 - A computer’s filesystem, consisting of files and directories
(folders). A file just contains text. A directory contains files
and subdirectories, each with a unique (for that directory) name.
The filesystem starts with a single, unnamed directory called
the “root.”

|#

#|

What are some operations we might want?

 - For both kinds of descendant tree:
    - Count or list all descendants.
    - Count or list descendants with a particular property (e.g., fleece
      color).
    - Find like-named descendants.
    - Make a picture of the tree.
    - Build a description of a particular descendant’s ancestry (path
      from the root of the tree).

 - For filesystems:
    - Given a piece of text, search for a file containing it and return its
      path.
    - Find a file or directory given a path (list of names) to it.
    - Create an empty directory given its name and path to a
      parent directory to create it in.
    - Create a file in an existing directory (or update an existing file),
      given its path and (new) contents.

|#


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

; An IntEvenTree is one of:
;  - (make-even-node IntOddTree Integer IntOddTree)
;
; An IntOddTree is one of:
;  - Integer
;  - (make-odd-node IntEvenTree Integer IntEvenTree)

#|

What is the template?

Can you?:

 - Sum all the integers in an IntEvenTree.
 - Return the largest integer in an IntEvenTree.
 - Sum only integers in the `even-node`s and ignore the others.
 - Check whether all the even-level integers (those in `even-node`s) are even
   and the odd-level integers (those in `odd-node`s or as leaves) are odd.

|#

; An Expression is one of:
;  - (make-plus-e [List-of Term])
;  - (make-minus-e Term Term)
;
; A Term is one of:
;  - (make-mult-t Term Factor)
;  - (make-div-t Term Factor)
;
; A Factor is one of:
;  - (make-lit-e Number)
;  - (make-var-e String)
;  - (make-paren-e Expression)
(define-struct plus-e [terms])
(define-struct minus-e [minuend subtrahend])
(define-struct mult-t [term factor])
(define-struct div-t [term factor])
(define-struct lit-e [value])
(define-struct var-e [name])
(define-struct paren-e [contents])

#|

What is the template?

Can you?:

 - Return a list of all the strings in all the `var-e`s of an Expression?

 - Given an Expression `e`, a String `s`, and a Number `n`, replace every
   occurrence of (make-var-e s) in `e` with (make-lit-e n).

 - Convert an Expression to a string, where `plus-e` turns into terms
   separated by " + ", `minus-e` separated by " - ", `mult-t` separated
   by " * ", `div-t` separated by " / ", `lit-e` shows the number, `var-e`
   shows the string, and `paren-e` puts parentheses around its contents.

 - Evaluate an expression to a number?

|#


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

#|

A text document has a title and a sequence of blocks. A block can be
a paragraph (containing inline text), a list (containing a non-empty
sequence of list items, each of which is a non-empty sequence of
blocks, or a heading (which has a level and some inline text), or a
block quote (which contains a non-empty sequence of blocks). Inline text
consists of a sequence of chunks, where each chunk can be a plain string,
an italic chunk (containing a sequence of sub-chunks), or a bold chunk
(also containing a sequence of sub-chunks).

Can you?:

  - Compute the textual length of a text document.
  - Append all its text into a string? With Markdown formatting? With HTML
    formatting.

|#



#|

An ISL program is a sequence of structure definitions, value definitions
(including constants and functions), and top-level expressions.
A structure definition gives the name of the structure and the names
of the fields. A value definition either defines a constant (has a name
and an expression) or a function (also has a name, one or more parameters,
and a body expresssion). A check is either a check-expect, which compares
the values of two expressions. An expression may be a literal value (such
as a string, number, or Boolean), a variable (represented by its name),
a primitive arithmetic expression (like + or string-append applied to two
expressions), a function application (consisting of a function name and
one or more argument expressions), a local expression (which has
a sequence of value definitions and a single body expression), or a cond
expression (which has a sequence of clauses, each of which consists of
a pair of expressions).

(We're leaving out: `else` in cond, `check-expect`, lots of other stuff.)

Can you?:

 - Count the number of definitions in a program.
 - Check that no sequence of definitions in a program tries to
   define the same name twice.
 - Check whether a program contains any undefined variables.
 - Run a program.

|#