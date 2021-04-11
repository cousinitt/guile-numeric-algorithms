#!/usr/bin/guile
!#

(define* (bisect-rootfind f a b #:optional #:key
                                           (tolerance 1e-10)
                                           (iterations 100))
  (define (converged? u v)
    (< (- u v) tolerance))

  (define (sign-difference? u v)
    (>= 0.0 (* u v)))

  (let loop ([a a]
             [fa (f a)]
             [b b]
             [fb (f b)]
             [iterations (1- iterations)])
    (let* ([midpoint (/ (+ a b) 2.0)]
           [fm (f midpoint)])
      (if (or (converged? b a)
              (<= iterations 0))
          midpoint
          (if (sign-difference? fa fm)
              (loop a fa midpoint fm (1- iterations))
              (loop midpoint fm b fb (1- iterations)))))))


; should be about 0.45158270528
(define (f x)
  (sin (cos (exp x))))

(let* ([x (bisect-rootfind f 0 1)]
       [fx (f x)])
  (format #t "x, f(x): ~s, ~s\n" x fx))
