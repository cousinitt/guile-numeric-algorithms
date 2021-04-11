#! /usr/bin/guile
!#

; Borrows heavily from:
; https://github.com/osveliz/numerical-veliz/blob/master/src/minimization/BrentJarratt.fsx
; Oscar's code is released with the MIT license.

; a:    left side of interval
; b:    right side of interval
; x:    best estimate
; w:    next best estimate
; v:    previous estimate
; dold: previous stepsize
; eold: previous golden interval

(define one-over-phi-squared (/ (- 3.0 (sqrt 5.0)) 2.0))

(define* (brent-minimize f a b #:optional #:key
                                          (tolerance 1e-10)
                                          (iterations 100))
  (let* ([x (+ a (* one-over-phi-squared (- b a)))]
         [w x]
         [v x])
    (let loop ([a a]
               [b b]
               [v x]
               [fv (f v)]
               [w w]
               [fw (f w)]
               [x x]
               [fx (f x)]
               [dold 0.0]
               [eold 0.0]
               [iterations (1- iterations)])
      (let ([midpoint (/ (+ a b) 2.0)])
        (if (or (<= (- b a) tolerance)
                (<= iterations 0))
            (cons midpoint (f midpoint))
            (let* ([r (* (- x w) (- fx fv))]
                   [tq (* (- x v) (- fx fw))]            ;; test ?
                   [tp (- (* (- x v) tq) (* r (- x w)))] ;; test ?
                   [tq2 (* 2.0 (- tq r))]                ;; test ?
                   [p (if (positive? tq2)
                          (- tp)
                          tp)]
                   [q (if (positive? tq2)
                          tq2
                          (- tq2))]
                   [safe? (not (zero? q))] ;; i.e., won't divide by zero
                   [deltax (if safe?
                               (/ p q)
                               0.0)]
                   [parabolic-step? (and safe?
                                    (< a (+ x deltax))
                                    (< (+ x deltax) b)
                                    (< (abs deltax) (/ (abs eold) 2.0)))]
                   [e (cond [parabolic-step? dold]
                            [(< x midpoint) (- b x)]
                            [else (- a x)])]
                   [d (if parabolic-step?
                          deltax
                          (* e one-over-phi-squared))]
                   [u (+ x d)]
                   [fu (f u)])
              (if (<= fu fx)
                  (let ([a (if (< u x)
                               a
                               x)]
                        [b (if (< u x)
                               x
                               b)])
                        (loop a b w fw x fx u fu d e (1- iterations)))
                  (let ([a (if (< u x)
                               u
                               a)]
                        [b (if (< u x)
                               b
                               u)])
                        (cond [(<= fu fw)
                               (loop a b w fw u fu x fx d e (1- iterations))]
                              [(or (<= fu fv)
                                   (= v x)
                                   (= v w))
                               (loop a b u fu w fw x fx d e (1- iterations))]
                              [else
                                (loop a b v fv w fw x fx d e (1- iterations))])))))))))


(define (f x)
  (let* ([w (- x 10.0)]
	 [z (/ (* w w) 50)])
    (- (exp (- z)))))


(let ([res (brent-minimize f 0 20)])
  (format #t "x, f(x): ~s, ~s\n" (car res) (cdr res)))
