;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; HW 5
;; Shivang Pant

; An AlpacaTree is one of:
;  - (make-alpaca String Sex Date Color AlpacaTree AlpacaTree)
;  - "unknown"
(define-struct alpaca [name sex dob color dam sire])
;
; where
;
; A Sex is one of:
;  - "female"
;  - "male"
;
; A Date is (make-date Year Month Day)
(define-struct date [year month day])
;   where
; Year is an Integer in [1900, 2019]
; Month is an Integer in [1, 12]
; Day is an Integer in [1, 31]

(define IRENE
  (make-alpaca "Irene of Acorn Alpacas"
               "female"
               (make-date 2007 5 21)
               "silver"
               (make-alpaca "MFA Independence"
                            "female"
                            (make-date 2002 7 2)
                            "black"
                            (make-alpaca "Dana Andrews"
                                         "female"
                                         (make-date 1996 8 14)
                                         "silver"
                                         "unknown"
                                         "unknown")
                            (make-alpaca "Jericho de Chuchata"
                                         "male"
                                         (make-date 1997 11 23)
                                         "black"
                                         "unknown"
                                         "unknown"))
               (make-alpaca "MA Sylvan"
                            "male"
                            (make-date 2001 5 16)
                            "black"
                            "unknown"
                            "unknown")))


(define IRENE2
  (make-alpaca "Irene of Acorn Alpacas"
               "female"
               (make-date 2007 5 21)
               "silver"
               (make-alpaca "MFA Independence"
                            "female"
                            (make-date 2002 7 2)
                            "black"
                            (make-alpaca "Dana Andrews"
                                         "female"
                                         (make-date 1996 8 14)
                                         "silver"
                                         "unknown"
                                         "unknown")
                            (make-alpaca "Jericho de Chuchata"
                                         "male"
                                         (make-date 2005 11 23)
                                         "black"
                                         "unknown"
                                         "unknown"))
               (make-alpaca "MA Sylvan"
                            "male"
                            (make-date 2002 5 16)
                            "black"
                            "unknown"
                            "unknown")))


(define IRENE3
  (make-alpaca "Irene of Acorn Alpacas"
               "female"
               (make-date 2007 5 21)
               "silver"
               (make-alpaca "MFA Independence"
                            "male"
                            (make-date 2002 5 2)
                            "black"
                            (make-alpaca "Dana Andrews"
                                         "female"
                                         (make-date 1996 8 14)
                                         "silver"
                                         "unknown"
                                         "unknown")
                            (make-alpaca "Jericho de Chuchata"
                                         "male"
                                         (make-date 1997 11 23)
                                         "black"
                                         "unknown"
                                         "unknown"))
               (make-alpaca "MA Sylvan"
                            "female"
                            (make-date 2002 5 16)
                            "black"
                            "unknown"
                            "unknown")))


(define IRENE4
  (make-alpaca "Irene of Acorn Alpacas"
               "female"
               (make-date 2007 5 21)
               "silver"
               (make-alpaca "MFA Independence"
                            "female"
                            (make-date 2002 7 2)
                            "black"
                            (make-alpaca "Dana Andrews"
                                         "female"
                                         (make-date 1996 8 14)
                                         "silver"
                                         "unknown"
                                         "unknown")
                            (make-alpaca "Jericho de Chuchata"
                                         "female"
                                         (make-date 1997 11 23)
                                         "black"
                                         "unknown"
                                         "unknown"))
               (make-alpaca "MA Sylvan"
                            "male"
                            (make-date 2001 5 16)
                            "black"
                            "unknown"
                            "unknown")))



(define ELIZABETH
  (make-alpaca "Elizabeth"
               "female" (make-date 1926 4 21) "red"
               "unknown" "unknown"))
(define CHARLES
  (make-alpaca "Charles"
               "male" (make-date 1948 11 14) "brown"
               ELIZABETH "unknown"))
(define MEGHAN
  (make-alpaca "Meghan"
               "female" (make-date 1981 8 4) "brown"
               "unknown" "unknown"))
(define HARRY
  (make-alpaca "Harry"
               "male" (make-date 1984 9 15) "red"
               "unknown" CHARLES))
(define ARCHIE
  (make-alpaca "Archie"
               "male" (make-date 2019 5 6) "blue"
               MEGHAN HARRY))






; Problem 1


; DATA DEFINITIONS

; A ListofDams is one of
; - (list String)
; - (list String ListofDams)


; female-line : AlpacaTree -> ListofDams
; Returns a list of the female line of ancestors from youngest to oldest
; using a the given AlpacaTree
; Examples:
(check-expect (female-line IRENE) (list "Irene of Acorn Alpacas"
                                        "MFA Independence"
                                        "Dana Andrews"))
(check-expect (female-line (alpaca-sire IRENE)) (list "MA Sylvan"))
; Strategy: Structural Decomposition
(define (female-line at)
  (cond
    [(string? (alpaca-dam at)) (list (alpaca-name at))]
    [else (append (list (alpaca-name at)) (female-line (alpaca-dam at)))]))



; Problem 2


; has-color? : AlpacaTree Color -> Boolean
; Checks an alpaca pedigree tree for a specific color
; Examples:
(check-expect (has-color? IRENE "blue") #false)
(check-expect (has-color? IRENE "black") #true)
(check-expect (has-color? ARCHIE "blue") #true)
; Strategy: Structural Decomposition
(define (has-color? at c)
  (cond
    [(string? at) #false]
    [else (or (string=? (alpaca-color at) c)
              (has-color? (alpaca-dam at) c)
              (has-color? (alpaca-sire at) c))]))



; Problem 3


; pedigree-error? : AlpacaTree -> Boolean
; Checks an alpaca pedigree for one of two errors:
; - Some alpaca in the tree has a birthday before one of its parents
; - A male alpaca is listed as a dam, or a female alpaca is listed as a sire.
; Examples:
(check-expect (pedigree-error? IRENE) #false)
(check-expect (pedigree-error? IRENE2) #true)
(check-expect (pedigree-error? IRENE4) #true)
(check-expect (pedigree-error? ARCHIE) #false)
; Strategy: Structural Decomposition
(define (pedigree-error? at)
  (cond
    [(string? at) #false]
    [else (or (check-damdate? at)
              (check-siredate? at)
              (check-genderdam at)
              (check-gendersire at)
              (pedigree-error? (alpaca-sire at))
              (pedigree-error? (alpaca-dam at)))]))
              





; check-damdate? : AlpacaTree -> Boolean
; Helper method for pedigree-error to check if an alpaca's dam has a birthday
; after itself
; Examples:
(check-expect (check-damdate? IRENE) #false)
(check-expect (check-damdate? (alpaca-sire IRENE)) #false)
;
; Strategy: Structural Decomposition
(define (check-damdate? at)
  (cond
    [(string? (alpaca-dam at)) #false]
    [else (bad-date? (alpaca-dob at) (alpaca-dob (alpaca-dam at)))]))


; check-siredate? : AlpacaTree -> Boolean
; Helper method for pedigree-error to check if an alpaca's sire has a birthday
; after itself
; Examples:
(check-expect (check-siredate? IRENE) #false)
(check-expect (check-siredate? (alpaca-sire IRENE)) #false)
;
; Strategy: Structural Decomposition
(define (check-siredate? at)
  (cond
    [(string? (alpaca-sire at)) #false]
    [else (bad-date? (alpaca-dob at) (alpaca-dob (alpaca-sire at)))]))


; bad-date? : Date Date -> Boolean
; Helper method for check-siredate? and check-damdate? that does the actual date
; checking
; Examples:
(check-expect (bad-date? (make-date 2002 11 5) (make-date 1999 11 5)) #false)
(check-expect (bad-date? (make-date 2002 11 5) (make-date 2005 11 5)) #true)
(check-expect (bad-date? (make-date 2002 11 5) (make-date 2002 5 2)) #false)
(check-expect (bad-date? (make-date 2002 5 5) (make-date 2002 7 6)) #true)
(check-expect (bad-date? (make-date 2002 5 5) (make-date 2002 5 27)) #true)
(check-expect (bad-date? (make-date 2002 5 5) (make-date 2002 5 3)) #false)
;(check-expect (bad-date (make-date 2002 11 5) 
; Strategy: Structural Decomposition
(define (bad-date? child adult)
  (cond
    [(> (date-year adult) (date-year child)) #true]
    [(< (date-year adult) (date-year child)) #false]
    [else (cond
            [(< (date-month adult) (date-month child)) #false]
            [(> (date-month adult) (date-month child)) #true]
            [else (cond
                    [(> (date-day adult) (date-day child)) #true]
                    [else #false])])]))


; check-genderdam : AlpacaTree -> Boolean
; Helper method for pedigree-error? that there is no error with the gender of
; the dams
; Examples:
(check-expect (check-genderdam IRENE) #false)
(check-expect (check-genderdam (alpaca-sire IRENE)) #false)
(check-expect (check-genderdam IRENE3) #true)
; Strategy: Structural Decomposition
(define (check-genderdam at)
  (cond
    [(string? (alpaca-dam at)) #false]
    [else (not (string=? "female" (alpaca-sex (alpaca-dam at))))]))


; check-gendersire : AlpacaTree -> Boolean
; Helper method for pedigree-error? that there is no error with the gender of
; the sires
; Examples:
(check-expect (check-gendersire IRENE) #false)
(check-expect (check-gendersire (alpaca-sire IRENE)) #false)
(check-expect (check-gendersire IRENE3) #true)
; Strategy: Structural Decomposition
(define (check-gendersire at)
  (cond
    [(string? (alpaca-sire at)) #false]
    [else (not (string=? "male" (alpaca-sex (alpaca-sire at))))]))




;; Problem 4


; oldest-ancestor : AlpacaTree -> AlpacaTree
; Takes a pedigree tree and goes back to return the oldest ancestor
; Examples:
(check-expect (oldest-ancestor IRENE) (make-alpaca "Dana Andrews"
                                         "female"
                                         (make-date 1996 8 14)
                                         "silver"
                                         "unknown"
                                         "unknown"))
(check-expect (oldest-ancestor ARCHIE) (make-alpaca "Elizabeth"
               "female" (make-date 1926 4 21) "red"
               "unknown" "unknown"))
; Strategy: Structural Decomposition
(define (oldest-ancestor at)
  (cond
    [(and (string? (alpaca-dam at))
         (string? (alpaca-sire at))) at]
    [(string? (alpaca-dam at)) (oldest-ancestor (alpaca-sire at))]
    [(string? (alpaca-sire at)) (oldest-ancestor (alpaca-dam at))]
    [else (older-alp (oldest-ancestor (alpaca-sire at))
                      (oldest-ancestor (alpaca-dam at)))]))



; older-alp : AlpacaTree AlpacaTree -> AlpacaTree
; Takes two alpacas and returns the older one
; Helper method for oldest-ancestor
; Examples:
(check-expect (older-alp IRENE (alpaca-dam IRENE)) (alpaca-dam IRENE))
(check-expect (older-alp (alpaca-dam IRENE) IRENE) (alpaca-dam IRENE))
(check-expect (older-alp (alpaca-dam IRENE2) (alpaca-sire IRENE2))
              (alpaca-sire IRENE2))
(check-expect (older-alp (alpaca-sire IRENE2) (alpaca-dam IRENE2))
              (alpaca-sire IRENE2))
(check-expect (older-alp (alpaca-dam IRENE3) (alpaca-sire IRENE3))
              (alpaca-dam IRENE3))
(check-expect (older-alp (alpaca-sire IRENE3) (alpaca-dam IRENE3))
              (alpaca-dam IRENE3))
; Strategy: Structural Decomposition
(define (older-alp at1 at2)
  (cond
    [(< (date-year (alpaca-dob at1)) (date-year (alpaca-dob at2))) at1]
    [(> (date-year (alpaca-dob at1)) (date-year (alpaca-dob at2))) at2]
    [else (cond
            [(< (date-month (alpaca-dob at1)) (date-month (alpaca-dob at2))) at1]
            [(> (date-month (alpaca-dob at1)) (date-month (alpaca-dob at2))) at2]
            [else (cond
                    [(< (date-day (alpaca-dob at1)) (date-day (alpaca-dob at2))) at1]
                    [(>= (date-day (alpaca-dob at1)) (date-day (alpaca-dob at2))) at2]
                    )])]))
    


;; Problem 5



;; A ListOfAlpacaTrees is one of
;; - (list AlpacaTree)
;; - (list AlpacaTree ListOfAlpacaTrees)


; all-ancestors/sorted : AlpacaTree -> ListofAlpacaTrees
; Takes an AlpacaTree and sorts all the alpacacs from youngest to oldest
; Examples:
(check-expect (all-ancestors/sorted IRENE)
  (cons
 (make-alpaca
  "Irene of Acorn Alpacas"
  "female"
  (make-date 2007 5 21)
  "silver"
  (make-alpaca
   "MFA Independence"
   "female"
   (make-date 2002 7 2)
   "black"
   (make-alpaca "Dana Andrews" "female" (make-date 1996 8 14) "silver"
                "unknown" "unknown")
   (make-alpaca "Jericho de Chuchata" "male" (make-date 1997 11 23)
                "black" "unknown" "unknown"))
  (make-alpaca "MA Sylvan" "male" (make-date 2001 5 16) "black"
               "unknown" "unknown"))
 (cons
  (make-alpaca
   "MFA Independence"
   "female"
   (make-date 2002 7 2)
   "black"
   (make-alpaca "Dana Andrews" "female" (make-date 1996 8 14) "silver"
                "unknown" "unknown")
   (make-alpaca "Jericho de Chuchata" "male" (make-date 1997 11 23) "black"
                "unknown" "unknown"))
  (cons
   (make-alpaca "MA Sylvan" "male" (make-date 2001 5 16) "black" "unknown"
                "unknown")
   (cons
    (make-alpaca "Jericho de Chuchata" "male" (make-date 1997 11 23) "black"
                 "unknown" "unknown")
    (cons (make-alpaca "Dana Andrews" "female" (make-date 1996 8 14) "silver"
                       "unknown" "unknown") '()))))))
(check-expect
  (all-ancestors/sorted ARCHIE)
  (list ARCHIE HARRY MEGHAN CHARLES ELIZABETH))
; Strategy: Structural Decomposition
(define (all-ancestors/sorted at)
  (cond
    [(and (string? (alpaca-dam at))
          (string? (alpaca-sire at))) (list at)]
    [(string? (alpaca-dam at)) (append (list at) (all-ancestors/sorted (alpaca-sire at)))]
    [(string? (alpaca-sire at)) (append (list at) (all-ancestors/sorted (alpaca-dam at)))]
    [else (append (list at) (merge-alpacas (all-ancestors/sorted (alpaca-dam at))
                                         (all-ancestors/sorted (alpaca-sire at))))]))
                         
       



; merge-alpacas : ListOfAlpacas ListOfAlpacas -> ListOfAlpacas
; Takes two sorted ListOfAlpacas and combines them into one sorted
; ListOfAlpacas, helper method for all-ancestors/sorted
; Strategy: Structural Decomposition
(define (merge-alpacas l1 l2)
  (cond
   [(empty? l1) l2]
   [(empty? l2) l1]
   [(first-younger? (first l1) (first l2))
    (merge-alpacas (rest l1) (append (list (first l1)) l2))]
   [else (append (list (first l2)) (merge-alpacas l1 (rest l2)))]))


; first-younger?: AlpacaTree AlpacaTree -> AlpacaTree
; Helper method for merge-alpacas that checks if the first alpaca
; given is younger than the second one
; Examples:
(check-expect (first-younger? IRENE (alpaca-dam IRENE)) #true)
(check-expect (first-younger? (alpaca-dam IRENE) IRENE) #false)
(check-expect (first-younger? (alpaca-dam IRENE2) (alpaca-sire IRENE2))
              #true)
(check-expect (first-younger? (alpaca-sire IRENE2) (alpaca-dam IRENE2))
              #false)
(check-expect (first-younger? (alpaca-dam IRENE3) (alpaca-sire IRENE3))
              #false)
(check-expect (first-younger? (alpaca-sire IRENE3) (alpaca-dam IRENE3))
              #true)
; Strategy: Strategic Decomposition
(define (first-younger? at1 at2)
  (cond
    [(< (date-year (alpaca-dob at1)) (date-year (alpaca-dob at2))) #false]
    [(> (date-year (alpaca-dob at1)) (date-year (alpaca-dob at2))) #true]
    [else (cond
            [(< (date-month (alpaca-dob at1)) (date-month (alpaca-dob at2))) #false]
            [(> (date-month (alpaca-dob at1)) (date-month (alpaca-dob at2))) #true]
            [else (cond
                    [(< (date-day (alpaca-dob at1)) (date-day (alpaca-dob at2))) #false]
                    [(>= (date-day (alpaca-dob at1)) (date-day (alpaca-dob at2))) #true]
                    )])]))


