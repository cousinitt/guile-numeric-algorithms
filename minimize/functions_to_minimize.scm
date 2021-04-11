#lang racket
(require plot)

(define (ackley x y)
  (+
   (* -20
      (exp (* -0.2
              (sqrt (/ (+ (sqr x) (sqr y)) 2)))))
   (- (exp (/ (+ (cos (* 2 pi x))
                 (cos (* 2 pi y))) 2)))
   (exp 1)
   20))

(plot3d (surface3d ackley -4 4 -4 4))

(define (rosenbrock x y)
  (+ (sqr (- 1 x))
     (sqr (- y (sqr x)))))

(plot3d (surface3d rosenbrock -2 2 -2 2))