#lang racket
(require gigls/unsafe)
(provide (all-defined-out))

;;; File:
;;;   lists-of-drawings-lab.scm
;;; Authors:
;;;   Samuel A. Rebelsky
;;;   Jerod Weinman
;;;   Mike Zou
;;; Summary:
;;;   Code for the lab entitled "Making and Manipulate Lists of Drawings"

; +--------------------+--------------------------------------------------------
; | Testing Procedures |
; +--------------------+

;;; Procedure:
;;;   check-drawing
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Renders drawing in an appropriately-sized image, making it useful 
;;;   for testing various drawings.
;;; Produces:
;;;   image, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   image is a new image
;;;   image contains a rendering of drawing
;;;   image is visible on the screen
(define check-drawing
  (lambda (drawing)
    (image-show 
     (drawing->image drawing 
                     (max 100
                          (inexact->exact (ceiling(drawing-right drawing))))
                     (max 100
                          (inexact->exact (ceiling (drawing-bottom drawing))))))))

;;; Procedure:
;;;   check-drawings
;;; Parameters:
;;;   drawings, a non-empty list of drawings [unverified]
;;; Purpose:
;;;   Renders all the drawings in an image, making it useful 
;;;   for testing.
;;; Produces:
;;;   image, an image
;;; Preconditions:
;;;   The check-drawing procedure is defined.
;;; Postconditions:
;;;   image is a new image
;;;   image contains a rendering of all the drawings in drawings.
;;;   image is visible on the screen
(define check-drawings
  (lambda (drawings)
    (check-drawing (drawing-compose drawings))))

; +--------------------+--------------------------------------------------------
; | Simple Computation |
; +--------------------+

(define times5 (lambda (x) (* 5 x)))
(define times10 (lambda (x) (* 10 x)))
(define times20 (lambda (x) (* 20 x)))

(define mod5 (lambda (x) (mod x 5)))
(define mod7 (lambda (x) (mod x 7)))
(define mod20 (lambda (x) (mod x 20)))

; +---------------+-------------------------------------------------------------
; | Sample Values |
; +---------------+

(define sample-circle 
  (vshift-drawing 
   50
   (scale-drawing 8 drawing-unit-circle)))

(define sample-square
  (vshift-drawing 25
                  (scale-drawing 6 drawing-unit-square)))

(define ten-circles (make-list 10 sample-circle))

(define list01 (list 0 10 20 30 40 50 60 70 80 90))
(define list02 (list 10 25 33 45 60 100 110 111 112 120))
(define x-circles-01 
  (map (section hshift-drawing <>  sample-circle)
       list01))
(define x-circles-04 
  (map (section vshift-drawing <> sample-circle)
       list02))
(define x-circles-07
  (map (lambda (h v)
         (hshift-drawing h (vshift-drawing v sample-circle)))
       list01
       list02))

(define list100 (list iota 50))

(check-drawings 
 (map (lambda (i)
        (hshift-drawing 
         i 
         (vshift-drawing
          i
           sample-square)))
      (iota 50)))
