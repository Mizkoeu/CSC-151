; Exam 2

#lang racket
(require gigls/unsafe)
(require rackunit)
(require rackunit/text-ui)

;;; File:
;;;   669581.rkt
;;; Authors:
;;;   The student currently referred to as 669581
;;;   Professor: Samuel A. Rebelsky
;;; Contents:
;;;   Code and solutions for Exam 2 2016S.

;; +---------+--------------------------------------------------------
;; | Grading |
;; +---------+

;; This section is for the grader's use.

;; Problem 1:   
;; Problem 2:   
;; Problem 3:   
;; Problem 4:   
;; Problem 5:   
;; Problem 6:   
;; Problem 7:   
;;           ----
;;     Total:   

;;    Scaled:   
;;    Errors:   
;;     Times:   
;;          :   
;;          :   
;;          :   
;;           ----
;;     Total:


;; +-----------+------------------------------------------------------
;; | Problem 1 |
;; +-----------+

; Time Spent: 8 min

; Citations:
; 1. CSC-151 Reading: Making and Manipulating Homogeneous Lists
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S
;    /readings/homogeneous-lists-reading.html
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/03/16. 

; Solution:

;;; Procedure:
;;;   color-names-much-bluer
;;; Parameters:
;;;   names, a list of strings
;;; Purpose:
;;;   Find the names of bluer versions of the colors in names
;;; Produces:
;;;   bluer-names, a list of strings
;;; Preconditions:
;;;   For all values, name, in names, (color-name? name) holds.
;;; Postconditions:
;;;   For all values, name in bluer-names, (color-name? name) holds.
;;;   (length bluer-names) = (length names)
;;;   For all i, 0 <= i < (length names)
;;;     Given that name-i is the ith element of names, and bluer-i is 
;;;     the ith element of bluer-names, 
;;;       bluer-i = 
;;;         (irgb->color-name (irgb-bluer (irgb-bluer (color->irgb name-i))))
(define color-names-much-bluer
  (lambda (names)
    (map (section (compose irgb->color-name 
                           rgb-bluer 
                           rgb-bluer 
                           color->irgb) <>) names))) 

; Examples/Tests:
; > (color-names-much-bluer (list "red" "blue" "green" "black" "white" "grey" "brown" "hotpink" "burlywood"))
; '("crimson"
;  "blue"
;  "forestgreen"
;  "midnightblue"
;  "white"
;  "mediumpurple"
;  "mediumvioletred"
;  "violet"
;  "thistle")

;; +-----------+------------------------------------------------------
;; | Problem 2 |
;; +-----------+

; Time Spent: 120 min (time spent on contemplation included).

; Citations:
; 1. CSC-151 Reading: Naming Values with Local Bindings
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/readings/let-reading.html
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/03/16.
; 2. Assignment 5: Drawing with Conditionals and Anonymous Procedures
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/assignments/assignment.05.html
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/03/16.

; Supplied procedure:

; (irgb-distance c1 c2)
;   A metric for the "distance" between two integer-encoded RGB colors.
(define irgb-distance
  (lambda (c1 c2)
    (inexact->exact
     (round
      (sqrt
       (+ (square (- (irgb-red c1) (irgb-red c2)))
          (square (- (irgb-green c1) (irgb-green c2)))
          (square (- (irgb-blue c1) (irgb-blue c2)))))))))

; Solution:

;;; Procedure:
;;;   irgb-speckle-4
;;; Parameters:
;;;   original, an integer-encoded RGB color
;;;   option1, an integer-encoded RGB color
;;;   option2, an integer-encoded RGB color
;;;   option3, an integer-encoded RGB color
;;;   option4, an integer-encoded RGB color
;;; Purpose:
;;;   output one of option1, option2, option3 and option4 based on 
;;;   probability (such that the closer the original color is to the
;;;   option, the higher the probability).
;;; Produces:
;;;   random-color, an integer-encoded RGB color
(define irgb-speckle-4 
  (lambda (original option1 option2 option3 option4)
    (let ([d1 (irgb-distance original option1)]
          [d2 (irgb-distance original option2)]
          [d3 (irgb-distance original option3)]
          [d4 (irgb-distance original option4)])
      (let ([dmax (max d1 d2 d3 d4)]
            [dmin (min d1 d2 d3 d4)])
        (let ([total (- (* 4 (+ dmax 1)) d1 d2 d3 d4)])
          (let ([rand (random total)])
            (cond [(< rand (- (+ dmax 1) d1)) 
                   option1]
                  [(< rand (- (* 2 (+ dmax 1)) d1 d2)) 
                   option2]
                  [(< rand (- (* 3 (+ dmax 1)) d1 d2 d3)) 
                   option3]
                  [else
                   option4])))))))


; Examples/Tests:
; (define kitten (image-load "/home/student/Desktop/kitten.jpg"))
(define boston (image-load "/home/zoutianh/Desktop/cityofboston.jpg"))
; > (image-show (image-variant kitten
;               (section irgb-speckle-4
;                        <>
;                        (irgb 255 0 0)
;                        (irgb 0 255 0)
;                        (irgb 0 0 255)
;                        (irgb 0 0 0))))
;120
;Success!
; > (image-show (image-variant kitten
;               (section irgb-speckle-4
;                        <>
;                        (irgb 128 0 0)
;                        (irgb 0 128 0)
;                        (irgb 0 0 128)
;                        (irgb 192 192 192))))
; 121
; > (image-show (image-variant kitten
;               (section irgb-speckle-4
;                        <>
;                        (irgb 192 192 0)
;                        (irgb 0 192 192)
;                        (irgb 192 0 192)
;                        (irgb 0 0 0))))
; 122
; > (image-show (image-variant boston
;               (section irgb-speckle-4
;                        <>
;                        (irgb 192 192 0)
;                        (irgb 0 192 192)
;                        (irgb 192 0 192)
;                        (irgb 0 0 0))))
; 123
; It works!!!


;; +-----------+------------------------------------------------------
;; | Problem 3 |
;; +-----------+

; Time Spent: 15 min

; Citations:
; 1. Assignment 4: Drawing Generally and Concisely
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/assignments
;    /assignment.04.html
;    I learned about how GIMP scales drawings from the problem sets: Smaller neighbor
;    Smaller-right-neighbor, etc. Assignment completed by student 669581 and partner.

; Solution:

;;; Procedure:
;;;   alternate-scale
;;; Parameters:
;;;   hpercent, a real number
;;;   vpercent, a real number
;;;   drawing, a drawing
;;; Purpose:
;;;   create a scaled drawing with left and top aligned to 
;;;   original drawing.
;;; Produces:
;;;   scaled-drawing, a drawing
;;; Preconditions:
;;;   0 <= hpercent <= 1
;;;   0 <= vpercent <= 1
;;; Postconditions:
;;;   (* (drawing-width drawing) hpercent) = (drawing-width scaled-drawing)
;;;   (* (drawing-height drawing) vpercent) = (drawing-height scaled-drawing)
;;;   (drawing-left drawing) = (drawing-left scaled-drawing)
;;;   (drawing-top drawing) = (drawing-top scaled-drawing) 
(define alternate-scale
  (lambda (hpercent vpercent drawing)
    (hshift-drawing
     (* (- 1 hpercent) (drawing-left drawing))
     (vshift-drawing 
      (* (- 1 vpercent) (drawing-top drawing))
      (hscale-drawing 
       hpercent 
       (vscale-drawing vpercent drawing)))))) 

; Examples/Tests:
(define test-drawing (hshift-drawing
                      50
                      (vshift-drawing
                       15
                       (scale-drawing 20 drawing-unit-circle))))
(define test-scale
  (test-suite
   "test of alternate-scale procedure"
   (test-case
    "scaled to right proportion?"
    (check-= (* (drawing-width test-drawing) .7) 
             (drawing-width (alternate-scale .7 .8 test-drawing)) 
             0.00001 "width")
    (check-= (* (drawing-height test-drawing) .8) 
             (drawing-height (alternate-scale .7 .8 test-drawing)) 
             0.00001 "height"))
   (test-case
    "Left side alignment"
    (check-= (drawing-left test-drawing) 
             (drawing-left (alternate-scale .7 .8 test-drawing)) 
             0.00001))
   (test-case
    "Top side alignment"
    (check-= (drawing-top test-drawing)
             (drawing-top (alternate-scale .7 .8 test-drawing)) 
             0.00001))))
; > (run-tests test-scale)
; 3 success(es) 0 failure(s) 0 error(s) 3 test(s) run
; 0
; Works so far. But more comprehensive test-suite will be written in the next problem.
; We'll see if the algorithm still works for other cases (shapes, positions, colors).


;; +-----------+------------------------------------------------------
;; | Problem 4 |
;; +-----------+

; Time Spent: 35 min

; Citations:
; 1. CSC-151 Laboratory: Unit Testing with RackUnit
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/labs/rackunit-rgb-lab.html
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/06/16.
; 2. CSC-151 Reading: Testing Your Procedures with RackUnit
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/readings/rackunit-rgb-reading.html
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/06/16.
; 3. CSC-151 Assignment 4: Assignment 4: Drawing Generally and Concisely
;    Problem 2: Testing Your Procedures
;    The test-suite code written by student 669581 and his partner was looked at.
;    The types of tests included in the code below were also guided by this problem.
;    Last viewed: 03/06/16.


; Provided code:

; (check-scaling message original scaled hscale vscale)
;   Determine if scaled is original, scaled by the appropriate
;   scale factors, and with the same top and left.
(define check-scaling
  (lambda (message original scaled hscale vscale)
    (test-case
     message
     (check-= (drawing-left scaled) (drawing-left original) 
              .001 
              "left")
     (check-= (drawing-top scaled) (drawing-top original) 
              .001
              "top")
     (check-= (drawing-width scaled) (* hscale  (drawing-width original)) 
              .001
              "width")
     (check-= (drawing-height scaled) (* vscale  (drawing-height original)) 
              .001
              "height")
     (check-equal? (drawing-color scaled) (drawing-color original)
                   "color")
     (check-equal? (drawing-type scaled) (drawing-type original)
                   "type"))))

; Solution:

(define alternate-scale-tests
  (test-suite
   "Tests of alternate-scale"
   (test-case
    "Unit square"
    (check-scaling "unit-square-same" 
                   drawing-unit-square
                   (alternate-scale 1 1 drawing-unit-square)
                   1 1)
    (check-scaling "unit-square-larger" 
                   drawing-unit-square
                   (alternate-scale 20 10 drawing-unit-square)
                   20 10)
    (check-scaling "unit-square-translated"
                   (hshift-drawing 20 (vshift-drawing 50 drawing-unit-square))
                   (alternate-scale 5 10 (hshift-drawing 
                                          20 
                                          (vshift-drawing 50 drawing-unit-square)))
                   5 10)
    (test-case
     "Rectangles"
     (check-scaling "rectangle-larger" 
                    (hscale-drawing 10 (vscale-drawing 15 drawing-unit-square))
                    (alternate-scale 5 10 (hscale-drawing 
                                           10 
                                           (vscale-drawing 15 drawing-unit-square)))
                    5 10)
     (check-scaling "rectangle-smaller" 
                    (hscale-drawing 10 (vscale-drawing 15 drawing-unit-square))
                    (alternate-scale .4 .7 (hscale-drawing 
                                            10 
                                            (vscale-drawing 15 drawing-unit-square)))
                    .4 .7)
     (let ([sample (hshift-drawing 
                    45
                    (vshift-drawing 
                     35 
                     (hscale-drawing 
                      10 (vscale-drawing 15 drawing-unit-square))))])
       (check-scaling "rectangle-translated" 
                      sample
                      (alternate-scale 5 10 sample)
                      5 10)))
    (test-case
     "Unit circle"
     (check-scaling "unit-circle-same"
                    drawing-unit-circle
                    (alternate-scale 1 1 drawing-unit-circle)
                    1 1)
     (check-scaling "unit-circle-larger"
                    drawing-unit-circle
                    (alternate-scale 30 40 drawing-unit-circle)
                    30 40)
     (check-scaling "unit-circle-translated"
                    (hshift-drawing 20 (vshift-drawing 50 drawing-unit-circle))
                    (alternate-scale 10 20 (hshift-drawing 
                                            20 
                                            (vshift-drawing 50 drawing-unit-circle)))
                    10 20))
    (test-case 
     "Ellipses"
     (check-scaling "ellipse-larger" 
                    (hscale-drawing 10 (vscale-drawing 15 drawing-unit-circle))
                    (alternate-scale 5 10 (hscale-drawing 
                                           10 
                                           (vscale-drawing 15 drawing-unit-circle)))
                    5 10)
     (check-scaling "ellipse-smaller" 
                    (hscale-drawing 10 (vscale-drawing 15 drawing-unit-circle))
                    (alternate-scale .75 .35 (hscale-drawing 
                                              10 
                                              (vscale-drawing 15 drawing-unit-circle)))
                    .75 .35)
     (let ([sample2 (hshift-drawing 
                     45
                     (vshift-drawing 
                      35 
                      (hscale-drawing 
                       10 (vscale-drawing 15 drawing-unit-circle))))])
       (check-scaling "ellipse-translated" 
                      sample2
                      (alternate-scale 5 10 sample2)
                      5 10))
     (test-case
      "Different colors?"
      (check-scaling "blue-circle"
                     (recolor-drawing "blue" drawing-unit-circle)
                     (alternate-scale 30 40 (recolor-drawing 
                                             "blue" drawing-unit-circle))
                     30 40))
     (test-case
      "Compound Drawings"
      (let ([thing1 (hshift-drawing 
                     60
                     (vshift-drawing 
                      75 
                      (hscale-drawing 
                       18 (vscale-drawing 21 drawing-unit-circle))))]
            [thing2 (hshift-drawing 
                     40
                     (vshift-drawing 
                      25 
                      (hscale-drawing 
                       15 (vscale-drawing 32 drawing-unit-square))))])
        (check-scaling "compound-drawing"
                       (drawing-group thing1 thing2)
                       (alternate-scale 15 32 (drawing-group thing1 thing2))
                       15 32)))))))

; Examples/Tests:
; > (run-tests alternate-scale-tests)
; 20 success(es) 0 failure(s) 0 error(s) 20 test(s) run
; 0


;; +-----------+------------------------------------------------------
;; | Problem 5 |
;; +-----------+

; Time Spent: 10 min

; Citations:
; 1. CSC-151 Reading: Building Images by Iterating Over Positions
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/readings
;    /iterate-positions-reading.html
;    The procedure of "image-compute" and its syntax was looked upon.
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/05/16.
; 2. CSC-151 Laboratory: Building Images by Iterating Over Positions
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/labs
;    /iterate-positions-lab.html
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/05/16.


; Solution:

;;; Procedure:
;;;   radial-green-blend
;;; Parameters:
;;;   width, a positive integer
;;;   height, a positive integer
;;;   radius, a positive integer
;;;   center-col, a non-negative integer
;;;   center-row, a non-negative integer
;;;   initial, an integer
;;;   final, an integer
;;; Purpose:
;;;   Create a width-by-height image that contains a green radial blend
;;;   centered at (center-col, center-row).
;;; Produces:
;;;   blend, an image
;;; Preconditions:
;;;   0 <= center-col < width
;;;   0 <= center-row < height
;;;   0 <= initial <= 255
;;;   0 <= final <= 255
;;; Postconditions:
;;;   (image-width blend) = width
;;;   (image-height blend) = height
;;;   For any position (i,j) in the image
;;;     (irgb-red (image-get-pixel blend i j)) = 0
;;;     (irgb-blue (image-get-pixel blend i j)) = 0
;;;   (irgb-green (image-get-pixel blend center-col center-row)) = initial
;;;   For any position (i,j) that is radius away from (center-col,center-row),
;;;     (irgb-green (image-get-pixel blend i j)) = final
;;;   For any other position less than radius away from the center, the 
;;;     green component is appropriately scaled between initial and final.
(define radial-green-blend
  (lambda (width height center-col center-row radius initial final)
    (let ([euclidean-distance 
           (lambda (col row)
             (sqrt (+ (square (- col center-col)) (square (- row center-row)))))])
      (image-compute 
       (lambda (col row)
         (if (< (euclidean-distance col row) radius)
             (irgb 0
                   (+ initial
                      (* (euclidean-distance col row) 
                         (- final initial) 
                         (/ radius)))
                   0 )
             (irgb 0 0 0)))
       width height))))

; Examples/Tests:
; > (image-show (radial-green-blend 256 256 64 64 128 0 255))
; 132
; > (image-show (radial-green-blend 256 256 64 64 128 255 0))
; 133
; It works!

;; +-----------+------------------------------------------------------
;; | Problem 6 |
;; +-----------+

; Time Spent: 90 min

; Citations:
; 1. CSC-151 Laboratory: Collage
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/labs/collage-lab.html
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/05/16.
; 2. CSC-151 Reading: Collage
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/readings/
;    collage-reading.html
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/05/16. 
; 3. GIMP Procedure Browser (Documentation for procedures regarding transformation,
;    scaling, and other image manipulation procedures).
;    Credit to the above mentioned reading: Collage, which directed me to this 
;    resource. Last viewed: 03/04/16.
; 4. CSC-151 Reading: Programming with GIMP Tools
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/readings
;    /gimp-tools-reading.html
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/07/16.

; Provided code:

; (alohraw image n)
;   Make an n-by-n copy of image, at the same width and height of image.
(define alohraw
  (lambda (source n)
    (let ([target (image-new (image-width source) (image-height source))])
      (map (section drella! source target n <>)
           (iota n))
      target)))

; Solution:

;;; Procedure:
;;;   drella!
;;; Parameters:
;;;   source, an image
;;;   target, an image
;;;   n, a positive integer
;;;   row, a non-negative integer
;;; Purpose:
;;;   Make a row of the specified number of n scaled copies of
;;;   the source image in the target image.
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;   (image-width target) = (image-width source)
;;;   (image-height target) = (image-height source)
;;;   (image-width source) is an integer multiple of n.  That is,
;;;     (integer? (/ (image-width source) n))
;;;   (image-height source) is an integer multiple of n.  That is,
;;;     (integer? (/ (image-height source) n))
;;;   0 <= row < n 
;;; Postconditions:
;;;   target now contains n scaled copies of source.
;;;   Each copy is 1/n the width and 1/n the height of source.
;;;   The copies are side-by-side, with the first copy's left edge
;;;     at the left edge of target and the last copy's right edge
;;;     at the right edge of target.
;;;   The top of each copy is row/n from the top of copy.
(define drella!
  (lambda (source target n row)  
    (let ([width (image-width source)]
          [height (image-height source)])
      (let ([new-width (/ width n)]
            [new-height (/ height n)])
        (image-select-rectangle! source REPLACE 0 0 width height)
        (gimp-edit-copy-visible source)
        (map (lambda (i)
               (image-select-rectangle! target REPLACE 
                                        (* i new-width) (* row new-height)
                                        new-width new-height)
               (let ([pasted (car (gimp-edit-paste (image-get-layer target) 1))])
                 (image-select-nothing! target)
                 (gimp-layer-scale pasted new-width new-height 1)
                 (gimp-image-flatten target)))
             (iota n))
        target))))

; Examples/Tests:
; > (drella! kitten kitten 5 1)
; 229
; > (drella! kitten boston 10 8)
; 230
; > (drella! boston boston 10 5)
; 231
; > (image-show (alohraw kitten 10))
; 232
; > (image-show (alohraw kitten 5))
; 233
; Judging by the number returned, it must've taken lots of trials and errors.
; But it works!

;; +-----------+------------------------------------------------------
;; | Problem 7 |
;; +-----------+

; Time Spent: 30 min

; Citations:
; 1. CSC-151 Laboratory: Scripting the GIMP Tools
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/labs/gimp-tools-lab.html
;    The syntax for selecting shapes, image REPLACE/ADD procedures for GIMP was looked at. 
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/06/16.
; 2. CSC-151 Reading: Programming with GIMP Tools
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/readings
;    /gimp-tools-reading.html
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/06/16.
; 3. CSC-151 Reading: Conditional Evaluation in Scheme
;    http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2016S/readings
;    /conditionals-reading.html
;    Referenced the syntax (square brackets, parentheses, etc) for writing conditionals.
;    By Curtsinger C., Davis J., Rebelsky S., and Weinman J. Last viewed: 03/06/16.

; Solution:

;;; Procedure:
;;;   add-speech-bubble
;;; Parameters:
;;;   image, an image
;;;   tail-index, an integer
;;;   left, a real number
;;;   right, a real number
;;;   top, a real number
;;;   bottom, a real number
;;; Purpose:
;;;   add a speech bubble onto the original image.
;;; Produces:
;;;   new-image, an image
;;; Preconditions:
;;;   left >= 0
;;;   right > left
;;;   top >= 0
;;;   bottom > top
;;; Postconditions:
;;;   if tail-index = 1, the upper-left of the bubble would be pointed corner,
;;;       while the rest of the three corners are rounded.
;;;   if tail-index = 2, the upper-right of the bubble would be pointed corner, 
;;;       while the rest of the three corners are rounded.
;;;   if tail-index = 4, the lower-right of the bubble would be pointed corner,
;;;       while the rest of the three corners are rounded.
;;;   if tail-index = 4, the lower-right of the bubble would be pointed corner,
;;;       while the rest of the three corners are rounded.
;;;   if tail-index equals any other integer, error message of "I don't like the value"
;;;      would be displayed.
;;;   (image-left image) = (image-left new-image)
;;;   (image-right image) = (image-right new-image)
;;;   (image-top image) = (image-top new-image)
;;;   (image-bottom image) = (image-bottom new-image)
;;;   (image-width new-image) = right - left
;;;   (image-height new-image) = bottom - top
;;;   The color of the speech-bubble created in the new-image is black (default 
;;;   foreground color).
(define add-speech-bubble 
  (lambda (image tail-index left top right bottom)
    (let ([width (- right left)]
          [height (- bottom top)])
      (image-select-ellipse! image REPLACE left top width height) 
      (cond [(= tail-index 1) 
             (image-select-rectangle! 
              image ADD left top (* 1/2 width) (* 1/2 height))] 
            [(= tail-index 2) 
             (image-select-rectangle! 
              image ADD left (+ top (* 1/2 width)) (* 1/2 width) (* 1/2 height))] 
            [(= tail-index 3)
             (image-select-rectangle! 
              image ADD (+ left (* 1/2 width)) top (* 1/2 width) (* 1/2 height))] 
            [(= tail-index 4) 
             (image-select-rectangle! 
              image ADD (+ left (* 1/2 width)) (+ top (* 1/2 height)) (* 1/2 width) (* 1/2 height))] 
            [else (error "I don't like the value" image)]) 
      (image-fill! image)
      (image-select-nothing! image) image)))

; Examples/Tests:
; > (image-show (jpt kitten 1 150 50 170 70))
; 133
; > (image-show (jpt kitten 2 100 50 130 80))
; 134
; > (image-show (jpt kitten 4 50 50 80 70))
; 135
; It works!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The End ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;