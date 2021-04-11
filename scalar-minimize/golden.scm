#!/usr/bin/guile
!#
(define golden-ratio (/ (+ (sqrt 5.0) 1.0) 2.0))


(define* (golden-section-minimize f a b #:optional #:key
                                                   (tolerance 1e-10)
                                                   (iterations 100))
  (let ([x1 (+ b (/ (- a b) golden-ratio))]
        [x2 (+ a (/ (- b a) golden-ratio))])
    (let loop ([a a]
               [x1 x1]
               [fx1 (f x1)]
               [x2 x2]
               [fx2 (f x2)]
	       [b b]
               [iterations (1- iterations)])
      (if (and (> (- b a) tolerance)
               (> iterations 0))
          (if (< fx1 fx2)
              (let* ([b x2]
                     [x2 x1]
		     [fx2 fx1]
	             [x1 (+ b (/ (- a b) golden-ratio))]
	             [fx1 (f x1)])
                (loop a x1 fx1 x2 fx2 b (1- iterations)))
              (let* ([a x1]
                     [x1 x2]
		     [fx1 fx2]
	             [x2 (+ a (/ (- b a) golden-ratio))]
                     [fx2 (f x2)])
                (loop a x1 fx1 x2 fx2 b (1- iterations))))
	  (if (< fx1 fx2)
	      (cons x1 fx1)
              (cons x2 fx2))))))


(define (f x)
  (let* ([w (- x 10.0)]
	 [z (/ (* w w) 50)])
    (- (exp (- z)))))


(let ([res (golden-section-minimize f 0 20)])
  (format #t "x, f(x): ~s, ~s\n" (car res) (cdr res)))
