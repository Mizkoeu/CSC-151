#lang racket
(require gigls/unsafe)
(provide (all-defined-out))


;;; Procedure:
;;;   lookup-color-by-component
;;; Parameters:
;;;   component, an integer between 0 and 255, inclusive
;;;   position, an integer between 1 and 3, inclusive
;;;   ctable, a list of color entries.
;;; Purpose:
;;;   Find a color name in the table which has the specified
;;;   component (1 for red, 2 for green, 3 for blue).
;;; Produces:
;;;   cname, a string (or the value #f)
;;; Preconditions:
;;;   Each entry in ctable must be a list.
;;;   Element 0 of each entry must be a string which represents a color name.
;;;   Elements 1, 2, and 3 of each entry must be integers which represent
;;;    the red, green, and blue components of each color, respectively.
;;; Postconditions:
;;;   If position is 1 and an entry with the same red value as component
;;;     appears somewhere in the table, cname is the name of one such entry.
;;;   If position is 2 and an entry with the same green value as component
;;;     appears somewhere in the table, cname is the name of one such entry.
;;;   If position is 3 and an entry with the same blue value as component
;;;     appears somewhere in the table, cname is the name of one such entry.
;;;   If no matching entries appear, cname is #f.
;;;   Does not affect the table.
(define lookup-color-by-component
  (lambda (component position ctable)
    (cond
      [(null? ctable)
       #f]
      [(equal? component (list-ref (car ctable) position))
       (car (car ctable))]
      [else
       (lookup-color-by-component component position (cdr ctable))])))


