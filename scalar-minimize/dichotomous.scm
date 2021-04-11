#!/usr/bin/guile
!#

(define* (dichotomous-minimize f a b #:optional #:key
                                                (tolerance 1e-10)
                                                (iterations 100))
  (let ([e (/ tolerance 2.0)])
    (let loop ([a a]
               [b b]
               [iterations (1- iterations)])
      (let* ([midpoint (/ (+ a b) 2.0)]
             [c (- midpoint e)]
             [fc (f c)]
             [d (+ midpoint e)]
             [fd (f d)])
        (if (or (< (/ (- b a) 2.0) tolerance)
                (<= iterations 0))
            (cons midpoint (f midpoint))
            (if (< fc fd)
                (loop a d (1- iterations))
                (loop c b (1- iterations))))))))


(define (f x)
  (let* ([w (- x 10.0)]
	 [z (/ (* w w) 50)])
    (- (exp (- z)))))


(let ([res (dichotomous-minimize f 0 20 #:iterations 3)])
  (format #t "x, f(x): ~s, ~s\n" (car res) (cdr res)))
