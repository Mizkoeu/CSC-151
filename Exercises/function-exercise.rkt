#lang racket
(require gigls/unsafe)

(define yield
  (lambda (val)
    (display " yields ")
    (display val)
    (newline)
    val))

(define irgb-max_*
  (lambda (color)
    (display "Taking the maximum component of the color ")
    (yield (irgb-max color))))

(define f1
  (lambda (color)
    (irgb (* (irgb-red color)
             (quotient (irgb-red color)
                       (irgb-max_* color)))
          (* (irgb-green color)
             (quotient (irgb-green color)
                       (irgb-max_* color)))
          (* (irgb-blue color)
             (quotient (irgb-blue color)
                       (irgb-max_* color))))))

(define f2
  (lambda (color)
    (f2-helper color (irgb-max_* color))))

(define f2-helper
  (lambda (color cmax)
    (irgb (* (irgb-red color)
             (quotient (irgb-red color) cmax))
          (* (irgb-green color)
             (quotient (irgb-green color) cmax))
          (* (irgb-blue color)
             (quotient (irgb-blue color) cmax)))))
