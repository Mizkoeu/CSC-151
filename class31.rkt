#lang racket

(define largest-abs-val
  (lambda (vals)
    (largest-abs-helper (car vals) (cdr vals))))


(define largest-abs-helper
  (lambda (largest-so-far remaining)
    (if (null? remaining)
        largest-so-far
        (largest-abs-helper (if (> (abs largest-so-far) (abs (car remaining)))
                                largest-so-far
                                (car remaining))
                            (cdr remaining)))))

(define lav
  (lambda (lst)
    (display (list 'lav lst)) (newline)
    (cond
      [(null? (cdr lst))
       (car lst)]
      [(>= (abs (car lst))
           (abs (lav (cdr lst))))
       (car lst)]
      [else
       (lav (cdr lst))])))
;> (lav '(1 2 3 4 5))
;(lav (1 2 3 4 5))
;(lav (2 3 4 5))
;(lav (3 4 5))
;(lav (4 5))
;(lav (5))
;(lav (5))
;(lav (4 5))
;(lav (5))
;(lav (5))
;(lav (3 4 5))
;(lav (4 5))
;(lav (5))
;(lav (5))
;(lav (4 5))
;(lav (5))
;(lav (5))
;(lav (2 3 4 5))
;(lav (3 4 5))
;(lav (4 5))
;(lav (5))
;(lav (5))
;(lav (4 5))
;(lav (5))
;(lav (5))
;(lav (3 4 5))
;(lav (4 5))
;(lav (5))
;(lav (5))
;(lav (4 5))
;(lav (5))
;(lav (5))
;5

;