#lang racket
(require gigls/unsafe)

(define world (image-show (image-new 500 500)))
(define tommy (turtle-new world))
(turtle-set-brush! tommy "2. Hardness 050" .5)

; There's no problem 1 according to the lastest version of assignment 6.

; 2)
(define turtle-polygon! 
  (lambda (t length sides)
    (turtle-teleport! t 50 50)
    (repeat sides 
            (lambda ()
              (turtle-forward! t length)
              (turtle-turn! t (/ 360 sides))))))

; 3)
;;; Procedure:
;;;   turtle-snowflake!
;;; Parameters:
;;;   turtle, a turtle
;;;   side-length, a number
;;;   sides, a positive integer
;;;   depth, a positive integer
;;; Purpose:
;;;   recursively draws a polygon with smaller polygons on the outside of each corner 
;;;   on the first polygin, even smaller polygons at the corners of the smaller polygons, 
;;;   and so on for depth levels.
;;; Produces:
;;;   flake-pattern, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   
(define turtle-snowflake!
  (lambda (turtle side-length sides depth)
    (letrec ([flakes!
              (lambda (l n)
                (if (<= n 1)
                    (repeat sides 
                            (lambda ()
                              (turtle-forward! turtle l)
                              (turtle-turn! turtle (/ 360 sides))))
                    (repeat sides 
                            (lambda ()
                              (turtle-forward! turtle l)
                              (turtle-turn! turtle (- (/ 360 sides) 180))
                              (flakes! (* .4 l) (- n 1))
                              (turtle-turn! turtle 180)))))])
      (turtle-teleport! turtle (* 50 depth) (* 50 depth))
      (flakes! side-length depth))))
