#lang racket
(require gigls/unsafe)


(image-compute
 (lambda (col row) 
   (irgb (* 255 (/ col 129)) 0 (* 255 (/ col 129))))
 129 65)

(image-compute
             (lambda (col row)
               (irgb (* col 2) 0 (* (- 65 row) 4)))
             129 65)

(image-compute
             (lambda (col row)
               (irgb (+ (* 128 (/ row 65)) (* 128 (/ col 64))) 0 0))
             65 64)
; Lab
; 3 (a)
(define horiz-black-to-blue
  (lambda (width height)
    (image-compute 
     (lambda (col row)
     (irgb 0 0 (* 255 (/ col width))))
      width height)))
; > (image-show (horiz-black-to-blue 200 200))
; 29

; (b)
(define horiz-blue-to-black
  (lambda (width height)
    (image-compute 
     (lambda (col row)
     (irgb 0 0 (- 255 (* 255 (/ col width)))))
      width height)))
; > (image-show (horiz-blue-to-black 100 150))
; 33

; (c)
(define horiz-light-to-dark-blue
  (lambda (width height)
    (image-compute 
     (lambda (col row)
     (irgb 0 0 (+ 64 (- 128 (* 128 (/ col width))))))
      width height)))
; > (image-show (horiz-light-to-darke-blue 100 100))
; 44

; (d)
(define horiz-dark-to-light-blue
  (lambda (width height)
    (image-compute 
     (lambda (col row)
     (irgb 0 0 (+ 64 (* 128 (/ col width)))))
      width height)))
; > (image-show (horiz-dark-to-light-blue 100 100))
; 48