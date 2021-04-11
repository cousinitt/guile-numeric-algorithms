#!/usr/bin/guile
!#

(define* (ternary-minimize f a b #:optional #:key
                                            (tolerance 1e-10)
                                            (iterations 100))
  (let loop ([a a]
             [b b]
             [iterations (1- iterations)])
    (let ([midpoint (/ (+ a b) 2.0)])
      (if (or (< (/ (- b a) 2.0) tolerance)
              (<= iterations 0))
          (cons midpoint (f midpoint))
          (let* ([c (/ (+ (* 2.0 a) b) 3.0)]
                 [fc (f c)]
                 [d (/ (+ a (* 2.0 b)) 3.0)]
                 [fd (f d)])
            (if (< fc fd)
                (loop a d (1- iterations))
                (loop c b (1- iterations))))))))


(define (f x)
  (let* ([w (- x 10.0)]
	 [z (/ (* w w) 50)])
    (- (exp (- z)))))


(let ([res (ternary-minimize f 0 20)])
  (format #t "x, f(x): ~s, ~s\n" (car res) (cdr res)))
