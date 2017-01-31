#lang racket
(require gigls/unsafe)
(provide (all-defined-out))

;;; File:
;;;   binary-search-lab.rkt
;;; Authors:
;;;   Janet Davis
;;;   Samuel A. Rebelsky
;;;   Jerod Weinman
;;;   YOUR NAME HERE
;;; Summary:
;;;   Starting code for the lab on binary search.

; +---------------+------------------------------------------------------------
; | Provided Data |
; +---------------+

;(define objects-by-name
;  (vector
;   (list "Amy"       "ellipse"   "blue"    90  50 25  5)
;   (list "Amy"       "rectangle" "blue"    90  50 25  5) 
;   (list "Amy"       "rectangle" "red"    90  50 25  5)
;   (list "Bob"       "ellipse"   "indigo"  80  40 35 30)
;   (list "Charlotte" "rectangle" "blue"     0  40  5 45)
;   (list "Danielle"  "rectangle" "red"      0 140 35 15)
;   (list "Devon"     "rectangle" "yellow"  80   0 10  5)
;   (list "Erin"      "ellipse"   "orange"  60  50 10 15)
;   (list "Fred"      "ellipse"   "black"    0 110 30 30)
;   (list "Greg"      "ellipse"   "orange" 110  10 35 50)
;   (list "Heather"   "rectangle" "white"  100 140 35 50)
;   (list "Ira"       "ellipse"   "red"    100 100  5 50)
;   (list "Janet"     "ellipse"   "black"   60  70  5 20)
;   (list "Karla"     "ellipse"   "yellow"  20 110 25 10)
;   (list "Leo"       "rectangle" "yellow"  60  40 30 50)
;   (list "Maria"     "ellipse"   "blue"    30  10  5 50)
;   (list "Ned"       "rectangle" "yellow"   0  50 45 15)
;   (list "Otto"      "rectangle" "red"    100  40 10 20)
;   (list "Otto"      "ellipse"   "red"    100  40 10 20)
;   (list "Otto"      "rectangle" "green"    100  40 10 20)
;   (list "Paula"     "ellipse"   "orange" 100  20 50 25)
;   (list "Quentin"   "ellipse"   "black"   40 130 35 50)
;   (list "Rebecca"   "rectangle" "green"  110  70 25 35)
;   (list "Sam"       "ellipse"   "white"   20 120 35 40)
;   (list "Ted"       "rectangle" "black"   20   0 10 20)
;   (list "Urkle"     "rectangle" "indigo"  40 110 10  5)
;   (list "Violet"    "rectangle" "violet"  80  80 50 20)
;   (list "Xerxes"    "rectangle" "blue"    60 130 25 35)
;   (list "Yvonne"    "ellipse"   "white"   40 110 50 40)
;   (list "Zed"       "rectangle" "grey"    90 60  25  5)
;  ))

(define objects-by-name
  (vector
   (list "Amy"       "ellipse"   "blue"    90  50 25  5)
   (list "Bob"       "ellipse"   "indigo"  80  40 35 30)
   (list "Charlotte" "rectangle" "blue"     0  40  5 45)
   (list "Danielle"  "rectangle" "red"      0 140 35 15)
   (list "Devon"     "rectangle" "yellow"  80   0 10  5)
   (list "Erin"      "ellipse"   "orange"  60  50 10 15)
   (list "Fred"      "ellipse"   "black"    0 110 30 30)
   (list "Greg"      "ellipse"   "orange" 110  10 35 50)
   (list "Heather"   "rectangle" "white"  100 140 35 50)
   (list "Ira"       "ellipse"   "red"    100 100  5 50)
   (list "Janet"     "ellipse"   "black"   60  70  5 20)
   (list "Karla"     "ellipse"   "yellow"  20 110 25 10)
   (list "Leo"       "rectangle" "yellow"  60  40 30 50)
   (list "Maria"     "ellipse"   "blue"    30  10  5 50)
   (list "Ned"       "rectangle" "yellow"   0  50 45 15)
   (list "Otto"      "rectangle" "red"    100  40 10 20)
   (list "Paula"     "ellipse"   "orange" 100  20 50 25)
   (list "Quentin"   "ellipse"   "black"   40 130 35 50)
   (list "Rebecca"   "rectangle" "green"  110  70 25 35)
   (list "Sam"       "ellipse"   "white"   20 120 35 40)
   (list "Ted"       "rectangle" "black"   20   0 10 20)
   (list "Urkle"     "rectangle" "indigo"  40 110 10  5)
   (list "Violet"    "rectangle" "violet"  80  80 50 20)
   (list "Xerxes"    "rectangle" "blue"    60 130 25 35)
   (list "Yvonne"    "ellipse"   "white"   40 110 50 40)
   (list "Zed"       "rectangle" "grey"    90 60  25  5)
  ))

(define objects-by-width
  (vector
   (list "Charlotte" "rectangle" "blue"     0  40  5 45)
   (list "Ira"       "ellipse"   "red"    100 100  5 50)
   (list "Janet"     "ellipse"   "black"   60  70  5 20)
   (list "Maria"     "ellipse"   "blue"    30  10  5 50)
   (list "Devon"     "rectangle" "yellow"  80   0 10  5)
   (list "Erin"      "ellipse"   "orange"  60  50 10 15)
   (list "Otto"      "rectangle" "red"    100  40 10 20)
   (list "Ted"       "rectangle" "black"   20   0 10 20)
   (list "Urkle"     "rectangle" "indigo"  40 110 10  5)
   (list "Amy"       "ellipse"   "blue"    90  50 25  5)
   (list "Karla"     "ellipse"   "yellow"  20 110 25 10)
   (list "Rebecca"   "rectangle" "green"  110  70 25 35)
   (list "Xerxes"    "rectangle" "blue"    60 130 25 35)
   (list "Zed"       "rectangle" "grey"    90 60  25  5)
   (list "Fred"      "ellipse"   "black"    0 110 30 30)
   (list "Leo"       "rectangle" "yellow"  60  40 30 50)
   (list "Bob"       "ellipse"   "indigo"  80  40 35 30)
   (list "Danielle"  "rectangle" "red"      0 140 35 15)
   (list "Greg"      "ellipse"   "orange" 110  10 35 50)
   (list "Heather"   "rectangle" "white"  100 140 35 50)
   (list "Quentin"   "ellipse"   "black"   40 130 35 50)
   (list "Sam"       "ellipse"   "white"   20 120 35 40)
   (list "Ned"       "rectangle" "yellow"   0  50 45 15)
   (list "Paula"     "ellipse"   "orange" 100  20 50 25)
   (list "Violet"    "rectangle" "violet"  80  80 50 20)
   (list "Yvonne"    "ellipse"   "white"   40 110 50 40)
  ))

; +---------------------+------------------------------------------------------
; | Provided Procedures |
; +---------------------+

;;; Procedure:
;;;   binary-search
;;; Parameters:
;;;   vec, a vector to search
;;;   get-key, a procedure of one parameter that, given a data item,
;;;     returns the key of a data item
;;;   may-precede?, a binary predicate that tells us whether or not
;;;     one key may precede another
;;;   key, a key we're looking for
;;; Purpose:
;;;   Search vec for a value whose key matches key.
;;; Produces:
;;;   match, a number.
;;; Preconditions:
;;;   The vector is "sorted".  That is,
;;;     (may-precede? (get-key (vector-ref vec i))
;;;                   (get-key (vector-ref vec (+ i 1))))
;;;     holds for all reasonable i.
;;;   The get-key procedure can be applied to all values in the vector.
;;;   The may-precede? procedure can be applied to all pairs of keys
;;;     in the vector (and to the supplied key).
;;;   The may-precede? procedure is transitive.  That is, if
;;;     (may-precede? a b) and (may-precede? b c) then it must
;;;     be that (may-precede? a c).
;;;   If two values are equal, then each may precede the other.
;;;   Similarly, if two values may each precede the other, then
;;;     the two values are equal.
;;; Postconditions:
;;;   If vector contains no element whose key matches key, match is -1.
;;;   If vec contains an element whose key equals key, match is the
;;;     index of one such value.  That is, key is 
;;;       (get-key (vector-ref vec match))
(define binary-search
  (lambda (vec get-key may-precede? key)
    ; Search a portion of the vector from lower-bound to upper-bound
    (let search-portion ([lower-bound 0]
                         [upper-bound (- (vector-length vec) 1)])
      (display lower-bound) (newline)
      (display upper-bound) (newline)
      ; If the portion is empty
      (if (> lower-bound upper-bound)
          ; Indicate the value cannot be found
          -1
          ; Otherwise, identify the middle point, the element at that 
          ; point and the key of that element.
          (let* ([midpoint (quotient (+ lower-bound upper-bound) 2)]
                 [middle-element (vector-ref vec midpoint)]
                 [middle-key (get-key middle-element)]
                 [left? (may-precede? key middle-key)]
                 [right? (may-precede? middle-key key)])
            (cond
              ; If the middle key equals the value, we use the middle value.
              [(and left? right?)
               midpoint]
              ; If the middle key is too large, look in the left half
              ; of the region.
              [left?
               (search-portion lower-bound (- midpoint 1))]
              ; Otherwise, the middle key must be too small, so look 
              ; in the right half of the region.
              [else
               (search-portion (+ midpoint 1) upper-bound)]))))))

; +-------+--------------------------------------------------------------------
; | Added |
; +-------+

; Lab Writeup:

;;; Procedure:
;;;   new-binary-search
;;; Parameters:
;;;   vec, a vector
;;;   get-key, a procedure that extracts keys from the elements of vec
;;;   may-precede?, a procedure that compares keys
;;;   key, a key to search for.
;;; Purpose:
;;;   Search vec for a value whose key matches key, and returns a “half value” if the value being searched for belongs 
;;;   between two neighboring value.
;;; Produces:
;;;   location, a number.
;;; Preconditions:
;;;   The vector is "sorted".  That is,
;;;     (may-precede? (get-key (vector-ref vec i))
;;;                   (get-key (vector-ref vec (+ i 1))))
;;;     holds for all reasonable i.
;;;   The get-key procedure can be applied to all values in the vector.
;;;   The may-precede? procedure can be applied to all pairs of keys
;;;     in the vector (and to the supplied key).
;;;   The may-precede? procedure is transitive.  That is, if
;;;     (may-precede? a b) and (may-precede? b c) then it must
;;;     be that (may-precede? a c).
;;;   If two values are equal, then each may precede the other.
;;;   Similarly, if two values may each precede the other, then
;;;     the two values are equal.
;;; Postconditions:
;;;   When vector contains no element whose key matches key, 
;;;     If key precedes the first key in vec, output -0.5.
;;;     If key comes after the last key in vec, output (- (vec-length vec) 0.5)
;;;     If the value being searched for belongs between two neighboring value
;;;        returns a “half value”.
;;;   If vec contains an element whose key equals key, match is the
;;;     index of one such value.  That is, key is 
;;;       (get-key (vector-ref vec match))
(define new-binary-search
  (lambda (vec get-key may-precede? key)
    (let search-portion ([lower-bound 0]a
                         [upper-bound (- (vector-length vec) 1)])
      (if (> lower-bound upper-bound)
          (exact->inexact (/ (+ lower-bound upper-bound) 2))
          (let* ([midpoint (quotient (+ lower-bound upper-bound) 2)]
                 [middle-element (vector-ref vec midpoint)]
                 [middle-key (get-key middle-element)]
                 [left? (may-precede? key middle-key)]
                 [right? (may-precede? middle-key key)])
            (cond
              [(and left? right?)
               midpoint]
              [left?
               (search-portion lower-bound (- midpoint 1))]
              [else
               (search-portion (+ midpoint 1) upper-bound)]))))))