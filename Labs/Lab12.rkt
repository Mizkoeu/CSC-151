#lang racket
(require gigls/unsafe)

;Lab-Writeup
;3.
;a. 
(define picname "/home/rebelsky/glimmer/samples/rebelsky-pic.jpg")
(image-show (image-load picname))
;b. 
(context-set-brush! "1. Pixel")

(define canvas (image-load picname))
(context-set-fgcolor! "black")
(image-draw-line! canvas 0 0 (image-width (image-load picname)) (image-height (image-load picname)))
(image-show canvas)