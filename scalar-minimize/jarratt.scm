#!/usr/bin/guile
!#

(define* (jarratt-minimize f a b #:optional #:key
                                            (tolerance 1e-10)
                                            (iterations 100))
  "Minimize supplied function by Jarratt's method."
  ; An iteration in Jarratt's method uses three known points:
  ; (x1, y1), (x2, y2), (x3, y3),
  ; to pick a fourth point, x4, using:
  ; x4 = x3 + 1/2 * ((x2 - x3)^2 * (y3 - y1) + (x1 - x3)^2 * (y2 - y3))/
  ;                 ((x2 - x3)   * (y3 - y1) + (x1 - x3)   * (y2 - y3)).
  (let loop ([x1 a]
	     [y1 (f a)]
	     [x2 (/ (+ a b))]
	     [y2 (f (/ (+ a b)))]
	     [x3 b]
	     [y3 (f b)]
             [iterations (1- iterations)])
    (let ([s (- x2 x3)]
          [t (- y3 y1)]
          [u (- x1 x3)]
	  [v (- y2 y3)])
      (let ([x4 (+ x3
		   (/ (+ (* s s t) (* u u v))
		      (+ (* s t) (* u v))
		      2))])
	(if (and (> (abs (- x4 x3)) tolerance)
                 (> iterations 0))
	    (loop x2 y2 x3 y3 x4 (f x4) (1- iterations))
	    (cons x4 (f x4)))))))


(define (f x)
  (let* ([w (- x 10.0)]
	 [z (/ (* w w) 50)])
    (- (exp (- z)))))


(let ([res (jarratt-minimize f 0 20)])
  (format #t "x, f(x): ~s, ~s\n" (car res) (cdr res)))
