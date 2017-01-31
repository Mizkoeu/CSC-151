#lang racket
(require gigls/unsafe)
(provide (all-defined-out))

;;; File:
;;;   analysis-lab.rkt
;;; Authors:
;;;   Janet Davis
;;;   Samuel A. Rebelsky
;;;   Jerod Weinman
;;;   YOUR NAME HERE
;;; Summary:
;;;   Procedures for the lab on analyzing procedures

; +--------------------------------+----------------------------------
; | Generalized Counter Procedures |
; +--------------------------------+

;;; Procedure:
;;;   counter-new
;;; Parameters:
;;;   name, a string
;;; Purpose:
;;;   Create a counter associated with the given name.
;;; Produces:
;;;   counter, a counter
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   counter can be used as a parameter to the various counter
;;;   procedures.
;;; Process:
;;;   Counters are two element vectors.  Element 0 is the name, and
;;;   should not change.  Element 1 is the count, and should change.
(define counter-new
  (lambda (name)
    (vector name 0)))

;;; Procedure:
;;;   counter-count!
;;; Parameters:
;;;   counter, a counter 
;;; Purpose:
;;;   count the counter
;;; Produces:
;;;   counter, the same counter, now mutated
;;; Preconditions:
;;;   counter was created by counter-new (or something similar) and
;;;   has only been modified by the counter procedures.
;;; Postconditions:
;;;   (counter-get counter) gives a number one higher than it 
;;;   did before.
(define counter-count!
  (lambda (counter)
    (vector-set! counter 1 (+ 1 (vector-ref counter 1)))
    counter))

;;; Procedure:
;;;   counter-get
;;; Parameters:
;;;   counter, a counter
;;; Purpose:
;;;   Get the number of times that counter-count! has been called
;;;   on this counter.
;;; Produces:
;;;   count, a non-negative integer
;;; Preconditions:
;;;   counter was created by counter-new and has only been modified
;;;   by the counter procedures.
;;; Postconditions:
;;;   count is the number of calls to counter-new on this counter since
;;;   the last call to counter-reset! on this counter, or since the
;;;   counter was created, if there have been no calls to counter-reset!
(define counter-get
  (lambda (counter)
    (vector-ref counter 1)))

;;; Procedure:
;;;   counter-reset!
;;; Parameters:
;;;   counter, a counter 
;;; Purpose:
;;;   reset the counter
;;; Produces:
;;;   counter, the same counter, now set to 0
;;; Preconditions:
;;;   counter was created by counter-new (or something similar) and
;;;   has only been modified by the other counter procedures.
;;; Postconditions:
;;;   (counter-get counter) gives 0.
(define counter-reset!
  (lambda (counter)
    (vector-set! counter 1 0)
    counter))

;;; Procedure:
;;;   counter-print!
;;; Parameters:
;;;   counter, a counter
;;; Purpose:
;;;   Print out the information associated with the counter.
;;; Produces:
;;;   counter, the same counter
;;; Preconditions:
;;;   counter was created by counter-new and has only been modified
;;;   by the various counter procedures.
;;; Postconditions:
;;;   counter is unchanged.
;;;   The output port now contains information on counter.
;;; Ponderings:
;;;   Why does counter-print! have a bang, given that it doesn't mutate
;;;   it's parameter?  Because it mutates the broader environment - we
;;;   call counter-print! not to compute a value, but to print something.
(define counter-print!
  (lambda (counter)
    (display (vector-ref counter 0))
    (display ": ")
    (display (vector-ref counter 1))
    (newline)))

; +-------------------+-----------------------------------------------
; | Specific Counters |
; +-------------------+

(define irgb-brightest-1-counter (counter-new "irgb-brightest-1"))
(define irgb-brightest-2-counter (counter-new "irgb-brightest-2"))
(define irgb-brightest-counters 
  (list irgb-brightest-1-counter irgb-brightest-2-counter))

; +-----------------------------+-------------------------------------
; | Finding the Brightest Color |
; +-----------------------------+

;;; Procedure:
;;;   all-irgb?
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   Determines whether lst contains only RGB colors.
;;; Produces:
;;;   okay?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (irgb? (list-ref lst i)) fails to hold for some valid i between
;;;     0 and (- (length lst) 1), okay? is false (#f).
;;;   Otherwise, okay? is true (#t)
(define all-irgb?
  (lambda (lst)
    (or (null? lst)
        (and (irgb? (car lst))
             (all-irgb? (cdr lst))))))

;;; Procedure:
;;;   irgb-brightness
;;; Parameters:
;;;   color, an integer-encoded RGB color
;;; Purpose:
;;;   Computes the brightness of color on a 0 (dark) to 100 (light) scale.
;;; Produces:
;;;   brightness, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If color1 is likely to be perceived as lighter than color2,
;;;     then (irgb-brightness color1) > (irgb-brightness color2).
;;;   0 <= brightness <= 100
(define irgb-brightness
  (lambda (color)
    (round (* 100 (/ (+ (* 0.30 (irgb-red color))
                        (* 0.59 (irgb-green color))
                        (* 0.11 (irgb-blue color)))
                     255)))))

;;; Procedure:
;;;   irgb-brighter
;;; Parameters:
;;;   color1, an integer-encoded RGB color.
;;;   color2, an integer-encoded RGB color.
;;; Purpose:
;;;   Find the brighter of color1 and color2.
;;; Produces:
;;;   brighter, an RGB color.
;;; Preconditions:
;;;   irgb-brightness is defined
;;; Postconditions:
;;;   brighter is either color1 or color2
;;;   (irgb-brightness brighter) >= (irgb-brightness color1)
;;;   (irgb-brightness brighter) >= (irgb-brightness color2)
(define irgb-brighter
  (lambda (color1 color2)
    (if (>= (irgb-brightness color1) (irgb-brightness color2))
        color1
        color2)))

;;; Procedure:
;;;   irgb-brighter?
;;; Parameters:
;;;   color1, a color
;;;   color2, a color
;;; Purpose:
;;;   Determine whether color1 is strictly brighter than color 2.
;;; Produces:
;;;   brighter?, a Boolean
;;; Preconditions:
;;;   irgb-brightness is defined.
;;; Postconditions:
;;;   If (irgb-brightness color1) > (irgb-brightness color2)
;;;     then brighter is true (#t)
;;;   Otherwise
;;;     brighter is false (#f)
(define irgb-brighter?
  (lambda (color1 color2)
    (> (irgb-brightness color1) (irgb-brightness color2))))

;;; Procedure:
;;;   irgb-brightest
;;; Parameters:
;;;   colors, a list of integer-encoded RGB colors
;;; Purpose:
;;;   Find the brightest color in the list.
;;; Produces:
;;;   brightest, an integer-encoded RGB color.
;;; Preconditions:
;;;   colors contains at least one element.
;;;   irgb-brightness is defined
;;; Postconditions:
;;;   For any color in colors,
;;;    (irgb-brightness color) <= (irgb-brightness brightest)
;;;   brightest is an element of colors.
(define irgb-brightest-1
  (lambda (colors)
    (counter-count! irgb-brightest-1-counter)
    (cond
      [(null? (cdr colors))
       (car colors)]
      [(irgb-brighter? (car colors) (irgb-brightest-1 (cdr colors)))
       (car colors)]
      [else
       (irgb-brightest-1 (cdr colors))])))
(define irgb-brightest-2
  (lambda (colors)
    (counter-count! irgb-brightest-2-counter)
    (if (null? (cdr colors))
        (car colors)
        (irgb-brighter (car colors)
                       (irgb-brightest-2 (cdr colors))))))
(define irgb-brightest-3
  (lambda (colors)
    (let kernel ([brightest-so-far (car colors)]
                 [remaining-colors (cdr colors)])
      (if (null? remaining-colors)
          brightest-so-far
          (kernel (irgb-brighter brightest-so-far (car remaining-colors))
                  (cdr remaining-colors))))))
(define irgb-brightest-4
  (lambda (colors)
    (when (not (all-irgb? colors))
      (error "irgb-brightest: expects a list irgb of colors; received" colors))
    (if (null? (cdr colors))
        (car colors)
        (irgb-brighter (car colors)
                       (irgb-brightest-4 (cdr colors))))))

; +-----------------+-------------------------------------------------
; | List Procedures |
; +-----------------+

;;; Procedure:
;;;   list-append
;;; Parameters:
;;;   front, a list of size n
;;;   back, a list of size m
;;; Purpose:
;;;   Put front and back together into a single list.
;;; Produces:
;;;   appended, a list of size n+m.
;;; Preconditions:
;;;   front is a list [Unverified]
;;;   back is a list [Unverified]
;;; Postconditions:
;;;   For all i, 0 <= i < n,
;;;    (list-ref appended i) is (list-ref front i)
;;;   For all i, <= i < n+m
;;;;   (list-ref appended i) is (list-ref back (- i n))
(define list-append
  (lambda (front back)
    (counter-count! list-append-counter)
    (if ($null? front)
        back
        ($cons ($car front) (list-append ($cdr front) back)))))

(define list-reverse-1
  (lambda (lst)
    (if ($null? lst)
        null
        (list-append (list-reverse-1 ($cdr lst)) 
                     ($cons ($car lst) null)))))

(define list-reverse-2
  (lambda (lst)
    (counter-count! list-reverse2-counter)
    (let kernel ([reversed null]
                 [remaining lst])
      (counter-count! list-kernel-counter)
      (if ($null? remaining)
          reversed
          (kernel ($cons ($car remaining) reversed)
                  ($cdr remaining))))))

(define list-append-counter (counter-new "list-append"))
(define list-kernel-counter (counter-new "kernel"))
(define list-reverse1-counter (counter-new "list-reverse1"))
(define list-reverse2-counter (counter-new "list-reverse2"))

(define car-counter (counter-new "car"))
(define cdr-counter (counter-new "cdr"))
(define cons-counter (counter-new "cons"))
(define null?-counter (counter-new "null?"))
(define list-counters (list car-counter cdr-counter cons-counter null?-counter))

(define $car
  (lambda (lst)
    (counter-count! car-counter)
    (car lst)))
(define $cdr
  (lambda (lst)
    (counter-count! cdr-counter)
    (cdr lst)))
(define $cons
  (lambda (val lst)
    (counter-count! cons-counter)
    (cons val lst)))
(define $null?
  (lambda (val)
    (counter-count! null?-counter)
    (null? val)))

; write up
; 1. How many calls to `cons` do you expect when you use 
; `list-reverse-1` to reverse a list of N values?  
; (You should give a formula in terms of N.)
; Ans: (N*(N+1))/2
; 2. How many calls to `cons` do you expect when you use
; `list-reverse-2` to reverse a list of N values?
; (You should give a formula in terms of N.)
; Ans: N
; 3. What do you take as the primary "morals" of this lab?
; Ans: We should try to write the most efficient algorithms that
; utilize the least number of procedure calls that do not take
; up too much of DrRacket system RAM.