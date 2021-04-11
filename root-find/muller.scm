#!/usr/bin/guile
!#
; This is apparently a good method for finding complex roots?
; TODO: The convergence is weird relative to the other methods.
;       Fix it so that it is similar.

(define* (muller-rootfind f a b #:optional #:key
                                           (tolerance 1e-10)
                                           (iterations 100))
  (define (sign x)
    (if (positive? x)
        -1.0
        1.0))

  ; Find the third needed point with the secant method.
  (define (secant-iteration x0 fx0 x1 fx1)
    ; x2 = x1 - f(x1)*(x1 - x0)/(f(x1) - f(x0))
    (- x1 (/ (* fx1 (- x1 x0))
             (- fx1 fx0))))

  (let* ([fa (f a)]
         [fb (f b)]
         [x (secant-iteration a fa b fb)]
         [fx (f x)])

    ; Muller's method iteration.
    (let loop ([x0 a]
               [fx0 (f a)]
               [x1 x]
               [fx1 fx]
               [x2 b]
               [fx2 (f b)]
               [iterations (1- iterations)])

      ; q = (x2 - x1)/(x1 - x0)
      ; a = q*f(x2) - q*(1 + q)*f(x1) + q*q*f(x0)
      ; b = (2*q + 1)*f(x2) - (1 + q)*(1 + q)*f(x1) + q*q*f(x0)
      ; c = (1 + q)*f(x2)
      ; x3 = x2 - (x2 - x1)*2*c/max(b +/- sqrt(b*b - 4*a*c))
      (let* ([q (/ (- x2 x1) (- x1 x0))]
             [qp1 (+ 1.0 q)]
             [a (+ (* q fx2)
                   (* -1.0 q qp1 fx1)
                   (* q q fx0))]
             [b (+ (* (+ (* 2.0 q) 1.0) fx2)
                   (* -1.0 qp1 qp1 fx1)
                   (* q q fx0))]
             [c (* qp1 fx2)]
             [d (sqrt (- (* b b) (* 4.0 a c)))] ; sqrt discriminant
             [x3 (- x2 (/ (* (- x2 x1) 2.0 c)
                          (+ b (* (- (sign b)) d))))])
        (if (or (< (abs (f x3)) tolerance)
                (<= iterations 0))
            x3
            (loop x1 fx1 x2 fx2 x3 (f x3) (1- iterations)))))))


; should be about 0.45158270528
(define (f x)
  (sin (cos (exp x))))

(let* ([x (muller-rootfind f 0 1)]
       [fx (f x)])
  (format #t "x, f(x): ~s, ~s\n" x fx))
