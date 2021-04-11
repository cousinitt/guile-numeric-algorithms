#!/usr/bin/guile
!#

(define* (regula-falsi-rootfind f a b #:optional #:key
                                                 (tolerance 1e-10)
                                                 (iterations 100))
  (define (sign-difference? u v)
    (>= 0.0 (* u v)))

  (define (converged? u v)
    (< (abs (- u v)) tolerance))

  (let loop ([x0 a]
             [fx0 (f a)]
             [x1 b]
             [fx1 (f b)]
             [iterations (1- iterations)])
    ; x2 = x1 - f(x1)*(x1 - x0)/(f(x1) - f(x0))
    (let* ([x2 (- x1
                  (/ (* fx1 (- x1 x0))
                     (- fx1 fx0)))]
           [fx2 (f x2)])
      (if (sign-difference? fx2 fx0)
          (if (or (converged? x0 x2)
                  (<= iterations 0))
              x2
              (loop x0 fx0 x2 fx2 (1- iterations)))
          (if (or (converged? x2 x1)
                  (<= iterations 0))
              x2
              (loop x2 fx2 x1 fx1 (1- iterations)))))))


; should be about 0.45158270528
(define (f x)
  (sin (cos (exp x))))

(let* ([x (regula-falsi-rootfind f 0 1)]
       [fx (f x)])
  (format #t "x, f(x): ~s, ~s\n" x fx))
