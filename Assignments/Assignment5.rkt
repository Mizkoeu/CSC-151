; Assignment 5

;--------------------------------------------------------------------------------
; Note: 
; We took more than 3.5 hours to complete this assignment!

; Citation:
;   1. CSC 151 Reading: Making and Manipulating Homogeneous Lists
; http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/readings/
; homogeneous-lists-reading.html
;   We looked at how to properly use map procedure and how to nest lists
;   into the procedure.
;   By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed 02/28/16
;   2. CSC 151 Laboratory: Writing Your Own Procedures
; http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/labs/
; procedures-rgb-lab.html
;   From Exercise 3: An Improved Greyscale Procedure
;   We took the codes from our previously written procedure (irgb-luma color)
;   to compute the luma of a color. 
;   By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed 02/28/16


#lang racket
(require gigls/unsafe)

; Problem 1 Drawing Stripes

; Define our initial square as follows:
; '(drawing rectangle 0 "" 0.0 0.0 1 1)
(define initial-square
  (hshift-drawing
   .5
   (vshift-drawing
    .5 drawing-unit-square)))

; Then we can build the procedure based on this initial square.
;;; Procedure:
;;;   make-stripes
;;; Parameters
;;;   num, an integer
;;;   color1, an integer-encoded RGB color
;;;   color2, an integer-encoded RGB color
;;; Purpose:
;;;   produce a stripe of two alternating colored squares.
;;; Produces:
;;;   stripe, a drawing
;;; Preconditions:
;;;   0 < num
;;; Postconditions:
;;;   (drawing-bottom stripe) = num
;;;   (drawing-top stripe) = 0
;;;   (drawing-left stripe) = 0
;;;   (drawing-right stripe) = 1
;;;   squares with even valued drawing-bottom is colored as "color1"
;;;   squares with odd numbered drawing-bottom is colored as "color2"
(define make-stripes
  (lambda (num color1 color2)
    (let ([test-color 
           (lambda (num)
             (if (even? num)
                 (recolor-drawing color1 initial-square)              
                 (recolor-drawing color2 initial-square)))])
      (drawing-compose
       (map vshift-drawing (iota num)    
            (map test-color (iota num)))))))

; Tests:
(image-show 
 (drawing->image 
  (hscale-drawing
   100 
   (vscale-drawing
    8 
    (make-stripes 9 "yellow" "red"))) 100 72)) 
(image-show 
 (drawing->image 
  (hscale-drawing
   120 
   (vscale-drawing
    8 
    (make-stripes 12 "blue" "white"))) 120 96)) 
; It works!


; Problem 2 Drawing Graphs

; First, define a list of x-coordinates of the centers of the circles.
(define list-x
  (map (compose (section + <> 5) (section * <> 10)) (iota 20)))
; the list given is as follows:
;'(5 15 25 35 45 55 65 75 85 95 105 115 125 135 145 155 165 175 185 195) 

;;; Procedure:
;;;   make-plot
;;; Parameters:
;;;   fun, a procedure
;;; Purpose:
;;;   create a drawing of circles colored and positioned according to a function.
;;; Produces:
;;;   fun-pattern, a drawing
;;; Preconditions:
;;;   0 < x < 200
;;;   -50 < (fun x) < 50
;;; Postconditions:
;;;   if (fun x) > 0, circles are colored as "blue";
;;;   if (fun x) < 0, circles are colored as "red".
;;;   x-coordinate of circle is according to "list-x"
;;;   y-coordinate of circle = 50 - (fun x)
(define make-plot
  (lambda (fun)
    (let ([change-color
           (lambda (x)
             (if (> (fun x) 0)
                 (recolor-drawing 
                  "blue" 
                  (scale-drawing 5 drawing-unit-circle))
                 (recolor-drawing 
                  "red" 
                  (scale-drawing 5 drawing-unit-circle))))])
      (drawing-compose
       (map hshift-drawing 
            list-x
            (map vshift-drawing 
                 (map (section - 50 <>) (map fun list-x))
                 (map change-color list-x)))))))


; Tests:  
; > (image-show (drawing->image (make-plot (lambda (x) (- (/ x 2) 50))) 200 100))
; 7
; > (image-show (drawing->image (make-plot (lambda (x) (* 50 (cos (/ x (* 4 pi)))))) 200 100))
; 8
; > (image-show (drawing->image (make-plot (lambda (x) (expt (/ (- x 100) 15) 3))) 200 100))
; 9
; Works.


; Problem 3 Sampling Images

; (a)

; First, define an (irgb-luma color) procedure:
(define irgb-luma
  (lambda (color)
    (+ (* (irgb-red color) .299)
       (* (irgb-green color) .587)
       (* (irgb-blue color) .114))))

;;; Procedure:
;;;   speckle-greyscale
;;; Parameters:
;;;   color, an integer-encoded RGB color
;;; Purpose:
;;;   takes in a color and returns either black or white.
;;; Produces:
;;;   new-color, an integer-encoded RGB color
;;; Preconditions:
;;;   [no additionals]
;;; Postconditions:
;;;   probability of new-color being "white" is (irgb-luma color)/256
;;;   probability of new-color being "black" is (256 - irgb-luma color)/256
(define speckle-greyscale
  (lambda (color)
    (if (< (random 256) (irgb-luma color))
        (irgb 255 255 255)
        (irgb 0 0 0))))

; Tests:
; > (image-show (image-variant (image-load "/home/rebelsky/kitten.jpg") speckle-greyscale))
; 15
; > (image-show (image-variant (image-load "/home/zoutianh/Desktop/cityofboston.jpg") speckle-greyscale))
; 16

; (b)

;;; Procedure:
;;;   speckle
;;; Parameters:
;;;   color, an integer-encoded RGB color
;;; Purpose:
;;;   outputs a color with each of the RGB components being either 255 or 0.
;;; Produces:
;;;   brand-new-color, an integer-encoded RGB color
;;; Preconditions:
;;;   [no additionals]
;;; Postconditions:
;;;   Probability of (irgb-red new-color) = 255 is (irgb-red color)/256
;;;   Probability of (irgb-green new-color) = 255 is (irgb-green color)/256
;;;   Probability of (irgb-blue new-color) = 255 is (irgb-blue color)/256
;;;   Each component can only be either 255 or 0.
(define speckle
  (lambda (color)
    (irgb (if (< (random 256) (irgb-red color))
              255
              0)
          (if (< (random 256) (irgb-green color))
              255
              0)
          (if (< (random 256) (irgb-blue color))
              255
              0))))
; Test:
; > (image-show (image-variant (image-load "/home/rebelsky/kitten.jpg") speckle))
; 17
; > (image-show (image-variant (image-load "/home/zoutianh/Desktop/cityofboston.jpg") speckle))
; 18
