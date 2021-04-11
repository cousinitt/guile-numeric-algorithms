(define (g01 x)
  "Test function: (x - 2.0)^2 + 1.0"
  (let ([y (- x 2.0)])
    (+ (* y y) 1.0)))


(define (g02 x)
  "Test function: x*x + exp(-x)"
  (+ (* x x) (exp (- x))))


(define (g03 x)
  "Test function: ((x*x + 2.0)*x + 1.0)*x + 3.0"
  (+ (* (+ (* (+ (*x x) 2.0) x) 1.0) x) 3.0))


(define (g04 x)
  "Test function: exp(x) + 0.01/x"
  (+ (exp x) (/ 1 x 100.0)))


(define (g05 x)
  "Test function: exp(x) - 2*x + 0.01/x - 0.000001/x^2"
  (- (+ (- (exp x) (* 2.0 x)) (/ 1 x 100)) (/ 1 x x 1000000.)))


(define (g06 x)
  "Test function: x*sin(10.0*pi*x) - 1.0"
  (let ([pi (* 4.0 (atan 1.0))])
    (- (* (sin (* 10.0 pi x)) x) 1.0)))
