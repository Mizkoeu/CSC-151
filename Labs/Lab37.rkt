; Lab 37
#lang racket
(require gigls/unsafe)
(provide (all-defined-out))

;;; File:
;;;   vectors-lab.rkt
;;; Authors:
;;;   Janet Davis
;;;   Samuel A. Rebelsky
;;;   Jerod Weinman
;;;   YOUR NAME HERE
;;; Summary:
;;;   Procedures for modifying number vectors

;;; Procedure:
;;;   number-vector-increment-index!
;;; Parameters:
;;;   vec, a vector
;;;   index, an integer
;;; Purpose:
;;;   Increment the value at a vector position
;;; Produces:
;;;   [Nothing; called for side effect.]
;;; Preconditions:
;;;   (vector-ref vec index) is a number
;;; Postconditions:
;;;   Let val be (vector-ref vec index) before the procedure call. After the
;;    call (vector-ref vec index) produces val+1.
(define number-vector-increment-index!
  (lambda (vec index)
    (vector-set! vec 
                 index 
                 (increment (vector-ref vec index)))))

;;; Procedure:
;;;   number-vector-increment!
;;; Parameters:
;;;   vec, a vector
;;; Purpose:
;;;   Increment the value at all vector positions
;;; Produces:
;;;   [Nothing; called for side effect.]
;;; Preconditions:
;;;   (vector-ref vec index) for 0 <= index < (vector-length vec) is a number.
;;; Postconditions:
;;;   Let val be (vector-ref vec index) before the procedure call. After the
;;    call (vector-ref vec index) produces val+1.
(define number-vector-increment!
  (lambda (vec)
    (let ([len (vector-length vec)]) ; unchanging value, tells recursion to stop
      (let kernel! ([pos 0])  ; Start the recursion at the first position
        (when (< pos len) ; When the position is valid,
          (number-vector-increment! vec pos) ; increment the number at pos
          (kernel! (+ 1 pos))))))) ; and process the rest of the vector

;;; Procedure:
;;;   number-vector-divide!
;;; Parameters:
;;;   vec, a vector
;;;   divisor, a number
;;; Purpose:
;;;   Divide the value at all vector positions by a given divisor
;;; Produces:
;;;   [Nothing; called for side effect.]
;;; Preconditions:
;;;   (vector-ref vec index) for 0 <= index < (vector-length vec) is a number.
;;; Postconditions:
;;;   Let val be (vector-ref vec index) before the procedure call. After the
;;    call (vector-ref vec index) produces val/divisor.
(define number-vector-divide!
  (lambda (vec divisor)
    (let ([len (vector-length vec)]) ; unchanging and tells recursion to stop
      (let kernel! ([pos 0])  ; Start the recursion at the first position
        (when (< pos len)     ; When the position is valid,
          (vector-set! vec    ; Set the new value in the vector
                       pos    ; at the current position
                       (/ (vector-ref vec pos) divisor)) ; to the quotient
          (kernel! (+ 1 pos))))))) ; and process the rest of the vector

;;; Procedure:
;;;   number-vector-largest
;;; Parameters:
;;;   vec, a vector
;;; Purpose:
;;;   Find the largest number in a vector
;;; Produces:
;;;   largest, a number
;;; Preconditions:
;;;   (vector-ref vec index) for 0 <= index < (vector-length vec) is a number.
;;; Postconditions:
;;;   (vector-ref vec index) <= largest for 0 <= index < (vector-length vec) 
;;;   largest is a value in vec, i.e., there exists an integer index such that
;;;      0 <= index < (vector-length vec)  and
;;;      (vector-ref vec index) = largest
(define number-vector-largest
  (lambda (vec)
    (let ([last (- (vector-length vec) 1)]) ; last position to test
      (let kernel ([pos 0])  ; Start the recursion at the first position
        (if (= pos last)
            ; We are at the last position, so return the number
            (vector-ref vec pos)
            ; Otherwise return the maximum of the current position and
            ; the largest number in the rest of the vector
            (max (vector-ref vec pos)
                 (kernel (+ 1 pos))))))))

; The initial code was written by Sam "R" Rebelsky, given as part of Laboratory: Vectors.
; It was viewed on 13 April 2016 and modified by us according to the requirements of Exercise 2.
(define vector-sum
  (lambda (vec)
    (let ([last (- (vector-length vec) 1)])
      (let kernel ([pos 0])
        (if (= pos (+ 1 last))
            0
            (+ (vector-ref vec pos)
                 (kernel (+ 1 pos))))))))
