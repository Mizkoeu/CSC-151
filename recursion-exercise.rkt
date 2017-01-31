#lang racket
(require gigls/unsafe)
(provide (all-defined-out))

;;; File:
;;;   helper-recursion-lab.scm
;;; Authors:
;;;   Janet Davis
;;;   Samuel A. Rebelsky
;;;   Jerod Weinman
;;;   Mike Zou
;;; Summary:
;;;   Code for the lab entitled "Recursion with Helper Procedures"


; +-----------------+-----------------------------------------------------------
; | Color Utilities |
; +-----------------+

;;; Procedure:
;;;   irgb-brightness
;;; Parameters:
;;;   color, an integer-encoded RGB color
;;; Purpose:
;;;   Computes the brightness of color on a 0 (dark) to 100 (light) scale.
;;; Produces:
;;;   b, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If color1 is likely to be perceived as lighter than color2,
;;;     then (brightness color1) > (brightness color2).
;;;   0 <= b <= 100
(define irgb-brightness
  (lambda (color)
    (round (* 100 (/ (+ (* 0.30 (irgb-red color))
                        (* 0.59 (irgb-green color))
                        (* 0.11 (irgb-blue color)))
                     255)))))

; +------------+----------------------------------------------------------------
; | Arithmetic |
; +------------+

;;; Procedure:
;;;   new-sum
;;; Parameters:
;;;   numbers, a list of numbers.
;;; Purpose:
;;;   Find the sum of the elements of a given list of numbers
;;; Produces:
;;;   total, a number.
;;; Preconditions:
;;;   All the elements of numbers must be numbers.
;;; Postcondition:
;;;   total is the result of adding together all of the elements of numbers.
;;;   If all the values in numbers are exact, total is exact.
;;;   If any values in numbers are inexact, total is inexact.
(define new-sum
  (lambda (numbers)
    (new-sum-helper 0 numbers)))

;;; Procedure:
;;;   new-sum-helper
;;; Parameters:
;;;   sum-so-far, a number.
;;;   remaining, a list of numbers.
;;; Purpose:
;;;   Add sum-so-far to the sum of the elements of a given list of numbers
;;; Produces:
;;;   total, a number.
;;; Preconditions:
;;;   All the elements of remaining must be numbers.
;;;   sum-so-far must be a number.
;;; Postcondition:
;;;   total is the result of adding together sum-so-far and all of the
;;;     elements of remaining.
;;;   If both sum-so-far and all the values in remaining are exact,
;;;     total is exact.
;;;   If either sum-so-far or any values in remaining are inexact,
;;;     total is inexact.
(define new-sum-helper
  (lambda (sum-so-far remaining)
    (if (null? remaining)
        sum-so-far
        (new-sum-helper (+ sum-so-far (car remaining))
                        (cdr remaining)))))

;;; Procedure:
;;;   difference
;;; Parameters:
;;;   lst, a list of numbers of the form (v1 v2 ... vn)
;;; Purpose:
;;;   Compute the difference of the values.
;;; Produces:
;;;   result, a number
;;; Preconditions:
;;;   lst is nonempty
;;;   lst contains only real numbers
;;; Postconditions:
;;;   result = v1 - v2 - v3 - ... - vn
(define difference
  (lambda (lst)
    (if (null? lst)
        0
        (- (car lst) (difference (cdr lst))))))

(define new-difference-helper
  (lambda (difference-so-far remaining)
    (if (null? remaining)
        difference-so-far
        (new-difference-helper (- difference-so-far (car remaining))
                               (cdr remaining)))))

(define new-difference
  (lambda (lst)
    (new-difference-helper 0 lst)))

(define newer-difference
  (lambda (lst)
    (new-difference-helper (car lst) (cdr lst))))

(define annotated-difference
  (lambda (lst)
    (annotated-difference-helper (car lst) (cdr lst))))
(define annotated-difference-helper
  (lambda (difference-so-far remaining)
    (write (list 'difference-helper difference-so-far remaining)) (newline)
    (if (null? remaining)
        difference-so-far
        (annotated-difference-helper (- difference-so-far (car remaining))
                                     (cdr remaining)))))

; +------------------+----------------------------------------------------------
; | Filtering Colors |
; +------------------+



(define irgb-filter-out-high-red
  (lambda (colors)
    (irgb-filter-out-high-red-helper null colors)))
(define irgb-filter-out-high-red-helper
  (lambda (colors-so-far remaining-colors)
    (cond
      ((null? remaining-colors)
       colors-so-far)
      ((<= 128 (irgb-red (car remaining-colors)))
       (irgb-filter-out-high-red-helper colors-so-far (cdr remaining-colors)))
      (else
       (irgb-filter-out-high-red-helper
        (append colors-so-far (list (car remaining-colors)))
        (cdr remaining-colors))))))

(define my-color-names 
  (list "palevioletred" "cadetblue" "darkseagreen" "goldenrod"
        "hotpink" "lightsteelblue" "burlywood" "mistyrose"
        "salmon" "peru" "plum" "turquoise"))
(define my-colors
  (map color-name->irgb my-color-names))
(define greys-4
  (map (lambda (n) (irgb (* 64 n) (* 64 n) (* 64 n)))
       (list 4 3 2 1)))
(define greys-8
  (map (lambda (n) (irgb (* 32 n) (* 32 n) (* 32 n)))
       (list 8 7 6 5 4 3 2 1)))
(define greys-16
  (map (lambda (n) (irgb (* 16 n) (* 16 n) (* 16 n)))
       (list 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))


(define product
  (lambda (num)
    (product-helper 1 num)))

(define product-helper
  (lambda (product-so-far remaining)
    (if (null? remaining)
        product-so-far
        (product-helper (* product-so-far (car remaining))
                        (cdr remaining)))))

(define my-quotient 
  (lambda (num)
    (quotient-helper (car num) (cdr num))))

(define quotient-helper
  (lambda (quotient-so-far remaining)
    (if (null? remaining)
        quotient-so-far
        (quotient-helper (/ quotient-so-far (car remaining))
                         (cdr remaining)))))


(define sum-red
  (lambda (colors)
    (sum-red-helper 0 colors)))
(define sum-red-helper 
  (lambda (sum-so-far remaining-colors)
    (if (null? remaining-colors)
        sum-so-far
        (sum-red-helper (+ sum-so-far (irgb-red (car remaining-colors)))
                        (cdr remaining-colors)))))


(define more-colors
  (map color-name->irgb (list "red" "green" "blue" "yellow" "black" "white")))

;
;(define irgb-brightest
;  (lambda (colors)
;    (display "irgb-brightest on list length ") 
;    (display (length colors)) 
;    (newline)
;    (cond
;      [(null? (cdr colors))
;       (car colors)]
;      [(>= (irgb-brightness (car colors)) 
;           (irgb-brightness (irgb-brightest (cdr colors))))
;       (car colors)]
;      [else
;       (irgb-brightest (cdr colors))])))

(define irgb-brightest
  (lambda (colors)
    (irgb-brightest-helper (car colors) (cdr colors))))
(define irgb-brightest-helper
  (lambda (brightest-so-far colors-remaining)
    (display "Brightest so far is ") 
    (display brightest-so-far)
    (newline)
    (display "length of remaining list ") 
    (display (length colors-remaining)) 
    (newline)
    (if (null? colors-remaining)
        brightest-so-far
        (irgb-brightest-helper
         (if (>= (irgb-brightness brightest-so-far) 
                 (irgb-brightness (car colors-remaining)))
             brightest-so-far
             (car colors-remaining))
         (cdr colors-remaining)))))

(define irgb-averaging
  (lambda (colors)
    (irgb-average-helper 0 0 0 0 colors)))
(define irgb-average-helper
  (lambda (red-so-far green-so-far blue-so-far count remaining-colors)
    (if (null? remaining-colors)
        (irgb (/ red-so-far count)
              (/ green-so-far count)
              (/ blue-so-far count))
        (irgb-average-helper (+ red-so-far 
                                (irgb-red (car remaining-colors)))
                             (+ green-so-far 
                                (irgb-green (car remaining-colors)))
                             (+ blue-so-far 
                                (irgb-blue (car remaining-colors)))
                             (+ 1 count)
                             (cdr remaining-colors)))))