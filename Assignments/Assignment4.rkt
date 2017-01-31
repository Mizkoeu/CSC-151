; Special Note:
; We have spent over 3.5 hours on this assignment.

#lang racket
(require gigls/unsafe)
(require rackunit)
(require rackunit/text-ui)

;Assignment 4

;Problem 1
;(a)
;;; Procedure:
;;;   simple-center-x
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Find the x-coordinate of the center of drawing.
;;; Produces:
;;;   val, a real number
;;; Precondition:
;;;   [no condition]
;;; Postcondition:
;;;   val = ((drawing-left drawing) + (drawing-right drawing)) / 2
(define simple-center-x 
  (lambda (drawing)
    (/ (+ (drawing-left drawing) 
          (drawing-right drawing)) 
       2)))

;(b)
;;; Procedure:
;;;   simple-center-x
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Find the y-coordinate of the center of drawing
;;; Produces:
;;;   val, a real number
;;; Precondition:
;;;   [no condition]
;;; Postcondition:
;;;   val = ((drawing-top drawing) + (drawing-bottom drawing)) / 2
(define simple-center-y
  (lambda (drawing)
    (/ (+ (drawing-top drawing) 
          (drawing-bottom drawing)) 
       2)))

; Tests:
;> (define pic 
;    (hshift-drawing
;     30
;     (vshift-drawing
;      30
;      (scale-drawing 
;       30 drawing-unit-circle))))
;> (simple-center-x pic)
;30.0
;> (simple-center-y pic)
;30.0
;> (simple-center-y pic)
;30.0

;Problem 2

(define simple-center-tests
  (test-suite 
   "Tests of simple-center"
   (test-case
    "Center is at origin"
    (check-= (simple-center-x drawing-unit-circle) 0 .00001)
    (check-= (simple-center-x drawing-unit-circle) 0 .00001))
   (test-case
    "Center along x-axis"
    (check-= (simple-center-x (hshift-drawing
                               50 drawing-unit-circle)) 50 .00001)
    (check-= (simple-center-y (hshift-drawing
                               50 drawing-unit-circle)) 0 .00001))
   (test-case
    "Center along y-axis"
    (check-= (simple-center-x (vshift-drawing
                               50 drawing-unit-circle)) 0 .00001)
    (check-= (simple-center-y (vshift-drawing
                               50 drawing-unit-circle)) 50 .00001))
   (test-case
    "Center in quadrant I"
    (check-= (simple-center-x (hshift-drawing
                               50 
                               (vshift-drawing 
                                50
                                drawing-unit-circle))) 50 .00001)
    (check-= (simple-center-y (hshift-drawing
                               50 
                               (vshift-drawing 
                                50
                                drawing-unit-circle))) 50 .00001)
    (test-case
     "Center in quadrant II"
     (check-= (simple-center-x (hshift-drawing
                                -50
                                (vshift-drawing
                                 50
                                 drawing-unit-circle))) -50 .00001)
     (check-= (simple-center-y (hshift-drawing
                                -50 
                                (vshift-drawing 
                                 50
                                 drawing-unit-circle))) 50 .00001))
    (test-case
     "Center in quadrant III"
     (check-= (simple-center-x (hshift-drawing
                                -50
                                (vshift-drawing 
                                 -50
                                 drawing-unit-circle))) -50 .00001)
     (check-= (simple-center-y (hshift-drawing
                                -50 
                                (vshift-drawing 
                                 -50
                                 drawing-unit-circle))) -50 .00001))
    (test-case
     "Center in quadrant IV"
     (check-= (simple-center-x (hshift-drawing
                                50
                                (vshift-drawing
                                 -50
                                 drawing-unit-circle))) 50 .00001)
     (check-= (simple-center-y (hshift-drawing
                                50 
                                (vshift-drawing 
                                 -50
                                 drawing-unit-circle))) -50 .00001))
    (test-case
     "Circles"
     (check-= (simple-center-x drawing-unit-circle) 0 .00001)
     (check-= (simple-center-x drawing-unit-circle) 0 .00001))
    (test-case
     "Squares"
     (check-= (simple-center-x drawing-unit-square) 0 .00001)
     (check-= (simple-center-x drawing-unit-square) 0 .00001))
    (test-case
     "Ellipses"
     (check-= (simple-center-x (hscale-drawing 6 drawing-unit-circle)) 0 .00001)
     (check-= (simple-center-x (hscale-drawing 6 drawing-unit-circle)) 0 .00001))
    (test-case
     "Rectangles"
     (check-= (simple-center-x (hscale-drawing 6 drawing-unit-square)) 0 .00001)
     (check-= (simple-center-x (hscale-drawing 6 drawing-unit-square)) 0 .00001))
    (test-case
     "Compound drawings"
     (check-= (simple-center-x (drawing-group (scale-drawing 10 drawing-unit-square)
                                              (hshift-drawing 
                                               5 
                                               (vshift-drawing 5 (scale-drawing 10 drawing-unit-circle)))))
              2.5 .00001)
     (check-= (simple-center-y (drawing-group (scale-drawing 10 drawing-unit-square)
                                              (hshift-drawing 
                                               5 
                                               (vshift-drawing 5 (scale-drawing 10 drawing-unit-circle)))))
              2.5 .00001))
    (test-case
     "Relatively small drawing"
     (check-= (simple-center-x drawing-unit-circle) 0 .00001)
     (check-= (simple-center-y drawing-unit-circle) 0 .00001))
    (test-case
     "Relatively large drawing"
     (check-= (simple-center-x (scale-drawing 1000 drawing-unit-circle)) 0 .00001)
     (check-= (simple-center-y (scale-drawing 1000 drawing-unit-circle)) 0 .00001)) 
    (test-case
     "Center coordinates are integers"
     (check-= (simple-center-x (hshift-drawing 
                                10
                                (vshift-drawing 20 drawing-unit-square))) 10 .00001)
     (check-= (simple-center-y (hshift-drawing 
                                10
                                (vshift-drawing 20 drawing-unit-square))) 20 .00001)
     (test-case
      "Center coordinates are not integers"
      (check-= (simple-center-x (hshift-drawing 
                                 10.8
                                 (vshift-drawing 16.4 drawing-unit-square))) 10.8 .00001)
      (check-= (simple-center-y (hshift-drawing 
                                 10.8
                                 (vshift-drawing 16.4 drawing-unit-square))) 16.4 .00001))))))

; Running the test suite:

; > (run-tests simple-center-tests)
; 16 success(es) 0 failure(s) 0 error(s) 16 test(s) run
; 0


;Problem 3
;(a)
;;; Procedure:
;;;   outline
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Create an "outline" of drawing.
;;; Produces:
;;;   my-outline, a drawing
;;; Preconditions:
;;;   No additional.
;;; Postconditions:
;;;   my-outline is 10% larger than drawing.  That is.
;;;     (drawing-width my-outline) = (* 11/10 (drawing-width drawing))
;;;     (drawing-height my-outline) = (* 11/10 (drawing-height drawing))
;;;   my-outline is the same "shape" as drawing.  
;;;   my-outline is colored grey.  
;;;   my-outline is centered at the same location as drawing.
;;;     (simple-center-x my-outline) = (simple-center-x drawing)
;;;     (simple-center-y my-outline) = (simple-center-y drawing)
(define outline
  (lambda (drawing)
    (hshift-drawing 
     (* -.1 (simple-center-x drawing))
     (vshift-drawing 
      (* -.1 (simple-center-y drawing))
      (scale-drawing 
       (/ 11 10) 
       (recolor-drawing "grey" drawing))))))

;(b)
(define pair-with-outline 
  (lambda (drawing) 
    (drawing-group
     (outline drawing)
     drawing)))
; Test:
; > (image-show (drawing->image
;               (pair-with-outline
;    (hshift-drawing
;     60
;     (vshift-drawing 
;      60
;      (scale-drawing 60 drawing-unit-circle)))) 100 100))
; 11


;(c)
(define pair-with-custom-outline 
  (lambda (drawing color hincrement vincrement)
    (drawing-group (hshift-drawing 
                    (* (- 0 hincrement) (simple-center-x drawing))
                    (vshift-drawing 
                     (* (- 0 vincrement) (simple-center-y drawing))
                     (hscale-drawing 
                      (+ 1 hincrement)
                      (vscale-drawing
                       (+ 1 vincrement)
                       (recolor-drawing color drawing)))))
                   drawing)))

;Test :
(define pic 
  (hshift-drawing
   60
   (vshift-drawing 
    60
    (scale-drawing 60 drawing-unit-circle))))
(define pic2 
  (hshift-drawing
   80
   (vshift-drawing 
    80
    (scale-drawing 100 drawing-unit-square))))
; > (image-show (drawing->image (pair-with-custom-outline pic "yellow" .5 .3) 100 100))
; 19
; > (image-show (drawing->image (pair-with-custom-outline pic2 "red" .1 .1) 200 200))
; 20

;(d)
; Documentation for pair-with-custom-outline
;;; Procedure
;;;   pair-with-custom-outline
;;; Parameters:
;;;   drawing, a drawing
;;;   color, an integer-encoded RGB color
;;;   hincrement, a real number
;;;   vincrement, a real number
;;; Purpose:
;;;   Create an "custom-outline" of drawing, that one can specify the color of the outline and how much bigger the outline is horizontally and vertically.
;;; Produces:
;;;   custom-outline, a drawing
;;; Precondition:
;;;   [No additional]
;;; Postcondition:
;;;   custom-outline is horizontally larger than drawing by hincrement. That is.
;;;     (drawing-width my-outline) = (* (+ 1 hincrement) (drawing-width drawing))
;;;   custom-outline is vertically larger than drawing by vincrement. That is.
;;;     (drawing-height my-outline) = (* (+ 1 vincrement) (drawing-height drawing))
;;;   custom-outline is the same "shape" as drawing.  
;;;   custom-outline is colored as (customized) color. 
;;;   custom-outline is centered at the same location as drawing.
;;;     (simple-center-x my-outline) = (simple-center-x drawing)
;;;     (simple-center-y my-outline) = (simple-center-y drawing)


;Problem 4
;(a)
;;; Procedure
;;;   smaller-right-neighbor
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose: 
;;;   create a slightly smaller neighbor to an image
;;; Produces;
;;;   neighbor, a drawing
;;; Preconditions:
;;;   None
;;; Postconditions:
;;;   (drawing-width neighbor) = 3/4 * (drawing-width drawing)
;;;   (drawing-height neighbor) = 3/4 * (drawing-height drawing)
;;;   (drawing-right drawing) = (drawing-left neighbor)
;;;   (drawing-top drawing) = (drawing-top neighbor)
;;;   neighbor is the same "shape" as drawing.
;;;   neighbor has same color as drawing.
(define smaller-right-neighbor
  (lambda (drawing)
    (hshift-drawing 
     (+ (* .25 (simple-center-x drawing))
        (* .5 (drawing-width drawing) 1.75))
     (vshift-drawing 
      (- (* .25 (simple-center-y drawing)) 
         (* .125 (drawing-height drawing)))
      (scale-drawing .75 drawing)))))

; Tests:
; Drawing thingy is previously defined as in the assignment.
(define thingy
  (hshift-drawing
   100
   (vshift-drawing
    60
    (scale-drawing
     80
     (drawing-group
      (recolor-drawing "black" drawing-unit-square)
      (recolor-drawing "red" drawing-unit-circle))))))

(define thingy2
  (hshift-drawing
   120
   (vshift-drawing 
    40
    (vscale-drawing 
     4
     (scale-drawing 50 (recolor-drawing "blue" drawing-unit-circle))))))

; > (image-show (drawing->image 
;               (drawing-group 
;                (smaller-right-neighbor thingy)
;                thingy)
;               200 100))
; 31
; > (= (drawing-width (smaller-right-neighbor thingy)) (* .75 (drawing-width thingy)))
; #t
; > (= (drawing-right thingy) (drawing-left (smaller-right-neighbor thingy)))
; #t
; > (image-show (drawing->image 
;                (drawing-group
;                 (smaller-right-neighbor thingy2)
;                 thingy2) 200 200))
; 11
; > (= (drawing-width (smaller-right-neighbor thingy2)) (* .75 (drawing-width thingy2)))
; #t
; > (= (drawing-right thingy2) (drawing-left (smaller-right-neighbor thingy2)))
; #t
; It works!

;(b)
(define add-scaled-right-neighbor
  (lambda (drawing scale-factor)
    (drawing-group
     (hshift-drawing 
      (+ (* (simple-center-x drawing) (- 1 scale-factor))
         (* .5 (drawing-width drawing) (+ 1 scale-factor)))
      (vshift-drawing 
       (- (* (simple-center-y drawing) (- 1 scale-factor)) 
          (* .5 (drawing-height drawing) (- 1 scale-factor)))
       (scale-drawing scale-factor drawing)))
     drawing)))

; Tests:
; > (image-show (drawing->image 
;                (add-scaled-right-neighbor thingy .25)
;                200 100))
; 38
; > (= (drawing-width (add-scaled-right-neighbor thingy)) (* .25 (drawing-width thingy)))
; #t
; > (= (drawing-right thingy) (drawing-left (add-scaled-right-neighbor thingy)))
; #t

; > (image-show (drawing->image 
;                (add-scaled-right-neighbor thingy2 1.8)
;                200 100))
; 39
; > (= (drawing-width (add-scaled-right-neighbor thingy2)) (* 1.8 (drawing-width thingy2)))
; #t
; > (= (drawing-right thingy2) (drawing-left (add-scaled-right-neighbor thingy2)))
; #t
; The algorithm does work.

;(c)
;;; Procedure: 
;;;   add-scaled-left-neighbor
;;; Parameters:
;;;   drawing, a drawing
;;;   scale-factor, a real number
;;; Purpose:
;;;   adds a neighbor, scaled to a certain proportion but with
;;;   same shape and color, to an image. 
;;; Produces:
;;;   neighbor, a drawing
;;; Preconditions:
;;;   scale-factor >= 0
;;; Postconditions:  
;;;   (drawing-width neighbor) = scale-factor * (drawing-width drawing)
;;;   (drawing-height neighbor) = scale-factor * (drawing-height drawing)
;;;   (drawing-left drawing) = (drawing-right neighbor)
;;;   (drawing-top drawing) = (drawing-top neighbor)
;;;   neighbor is the same "shape" as drawing.
;;;   neighbor has same color as drawing.   
(define add-scaled-left-neighbor
  (lambda (drawing scale-factor)
    (drawing-group
     (hshift-drawing 
      (- (* (simple-center-x drawing) (- 1 scale-factor))
         (* .5 (drawing-width drawing) (+ 1 scale-factor)))
      (vshift-drawing 
       (- (* (simple-center-y drawing) (- 1 scale-factor)) 
          (* .5 (drawing-height drawing) (- 1 scale-factor)))
       (scale-drawing scale-factor drawing)))
     drawing)))


;(d)
(define add-scaled-bottom-neighbor
  (lambda (drawing scale-factor)
    (drawing-group
     (hshift-drawing 
      (* (simple-center-x drawing) (- 1 scale-factor))
      (vshift-drawing 
       (+ (* (simple-center-y drawing) (- 1 scale-factor)) 
          (* .5 (drawing-height drawing) (+ 1 scale-factor)))
       (scale-drawing scale-factor drawing)))
     drawing)))

; Tests:
; > (image-show (drawing->image
;                (add-scaled-bottom-neighbor thingy2 .75)
;                200 200))
; 47
; > (= (drawing-width (add-scaled-bottom-neighbor thingy2)) (* .75 (drawing-width thingy2)))
; #t
; > (= (drawing-right thingy2) (drawing-left (add-scaled-bottom-neighbor thingy2)))
; #t
; > (= (drawing-bottom thingy2) (drawing-top (add-scaled-bottom-neighbor thingy2)))
; #t

;Problem 5
;;; Procedure:
;;;   circle-y-coord
;;; Parameters:
;;;   i, an integer
;;; Purpose:
;;;   computes the y-coordinate of the i-th point around the circle.
;;; Produces:
;;;   y-coordinate, a real number
;;; Precondition:
;;;   0 <= i < 36
;;; Postcondition:
;;;   (sin (((i * 10)/180) * pi))* 80 = y-coordinate
(define circle-y-coord 
  (lambda (i)
    (+ 100 (* 80 (sin (* pi (/ (* i 10) 180)))))))

;;; Procedure:
;;;   circle-x-coord
;;; Parameters:
;;;   i, an integer
;;; Purpose:
;;;   computes the x-coordinate of the i-th point around the circle.
;;; Produces:
;;;   x-coordinate, a real number
;;; Precondition:
;;;   0 <= i < 36
;;; Postcondition:
;;;   (cos (((i * 10)/180) * pi))* 80 = x-coordinate
(define circle-x-coord 
  (lambda (i)
    (+ 100 (* 80 (cos (* pi (/ (* i 10) 180)))))))

;;; Attempt to recreate the circle from 36 copies of smaller circles: 
(define circle-of-drawings
  (lambda (drawing)
    (map (lambda (i)
           (hshift-drawing 
            (circle-y-coord i)
            (vshift-drawing
             (circle-x-coord i)
             drawing)))
         (iota 36))))
; Test:
; (define c5 (scale-drawing 5 drawing-unit-circle))
; (image-show (drawing->image (drawing-compose (circle-of-drawings c5)) 200 200))
; 1
; GIMP shows the same image as presented in the problem set. (Except that the radius of the 
; circle is arbitrarily set to 80, due to insufficient information.)


;Problem 6 Make Your Own Drawings

; Making a procedure that arranges 12 drawings in a circle.
(define dot-field
  (lambda (drawing)
    (map (lambda (i)
           (hshift-drawing 
            (+ 100 (* 85 (cos (* (/ i 6) pi))))
            (vshift-drawing
             (+ 100 (* 85 (sin (* (/ i 6) pi))))
             drawing)))
         (iota 12))))

; drawing the MAIN meteorite, with color gradient.
(define meteorite
  (drawing-compose (map (lambda (i) 
                          (hshift-drawing 
                           i
                           (vshift-drawing 
                            i
                            (scale-drawing 
                             i 
                             (recolor-drawing (irgb (+ 50 i i) 20 20) drawing-unit-circle)))))
                        (map increment (iota 100)))))

; drawing the background meteorites, with color gradient.
(define mini-meteo
  (drawing-compose (map (lambda (i) 
                          (hshift-drawing 
                           i
                           (vshift-drawing 
                            i
                            (scale-drawing 
                             i 
                             (recolor-drawing (irgb (+ 170 i i i) 30 70) drawing-unit-circle)))))
                        (map increment (iota 20)))))

; Showing the image of meteorites.
(image-show (drawing->image 
             (drawing-group (drawing-compose
                             (dot-field mini-meteo))
                            meteorite) 200 200))

; Drawing Doraemon!!!

(define doraemon (image-new 200 200))

; Draw the primary circle Head
(image-select-ellipse! doraemon REPLACE
                       10 10 180 180)
(context-set-fgcolor! "blue")
(image-fill-selection! doraemon)

(context-set-fgcolor! "black")
(context-set-brush! "2. Hardness 100" 1)
(image-stroke-selection! doraemon)

; Draw the secondary oval Face
(image-select-ellipse! doraemon REPLACE
                       20 44 160 150)
(context-set-fgcolor! "white")
(image-fill-selection! doraemon)
(context-set-fgcolor! "black")
(context-set-brush! "2. Hardness 100" 0.5)
(image-stroke-selection! doraemon)

; Draw Eyes
(image-select-ellipse! doraemon REPLACE
                       68 28 34 48)
(context-set-fgcolor! "white")
(image-fill-selection! doraemon)
(context-set-fgcolor! "black")
(context-set-brush! "2. Block 03" 1.5)
(image-stroke-selection! doraemon)

(image-select-ellipse! doraemon REPLACE
                       102 28 34 48)
(context-set-fgcolor! "white")
(image-fill-selection! doraemon)
(context-set-fgcolor! "black")
(context-set-brush! "2. Block 03" 1.5)
(image-stroke-selection! doraemon)

(image-select-ellipse! doraemon REPLACE
                       85 50 8 13)
(image-select-ellipse! doraemon ADD
                       110 50 8 13)
(context-set-fgcolor! "black")
(image-fill-selection! doraemon)

; Draw round nose
(image-select-ellipse! doraemon REPLACE
                       95 64 15 15)
(image-select-ellipse! doraemon SUBTRACT
                       96 68 7 7)
(context-set-fgcolor! "red")
(image-fill-selection! doraemon)
(context-set-fgcolor! "black")
(context-set-brush! "2. Block 03" 1.5)
(image-stroke-selection! doraemon)

; Draw mouth
(image-select-ellipse! doraemon REPLACE
                       40 68 120 110)
(image-select-rectangle! doraemon SUBTRACT
                         40 20 120 100)
(context-set-fgcolor! (irgb 140 15 10))
(image-fill-selection! doraemon)
(context-set-brush! "2. Hardness 100" 2)
(image-stroke-selection! doraemon)

(image-select-ellipse! doraemon REPLACE
                       25 150 105 100)
(image-select-ellipse! doraemon ADD
                       70 150 105 100)
(image-select-ellipse! doraemon INTERSECT
                       40 68 120 110)
(context-set-fgcolor! (irgb 250 15 15))
(image-fill-selection! doraemon)
(context-set-brush! "2. Hardness 100" 2)
(image-stroke-selection! doraemon)

;Show the combination of drawings. Voila our doraemon!!:D
(image-show doraemon)