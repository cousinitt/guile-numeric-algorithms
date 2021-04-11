#!/usr/bin/guile
!#
; This website was great for describing the flow of the algorithm:
; https://codetobuy.com/downloads/brent-dekker-method-as-a-root-finding-method-of-any-equation/

(use-modules (ice-9 receive))

; https://en.wikipedia.org/wiki/Machine_epsilon
; https://bitbashing.io/comparing-floats.html
; The value returned here seems close to DBL_EPSILON from 'float.h'.
(define machine-epsilon
  (let loop ([e 1.0])
    (if (not (= (+ 1.0 (/ e 2.0)) 1.0))
        (loop (/ e 2.0))
        e)))


(define (near? a b)
  (< (abs (- a b)) machine-epsilon))


(define (sign-difference? u v)
  (< (* u v) 0.0))


(define (between? a x b)
  (let ([interval-size (abs (- b a))])
    (and (< (abs (- b x)) interval-size)
         (< (abs (- x a)) interval-size))))


(define (sort-interval a fa b fb)
  (if (< (abs fa) (abs fb))
      (values b fb a fa)
      (values a fa b fb)))


(define (root-interpolate a fa b fb c fc)
  (if (and (not (near? fa fc))
           (not (near? fb fc)))
      (+ (/ (* a fb fc)
            (- fa fb)
            (- fa fc))
         (/ (* b fa fc)
            (- fb fa)
            (- fb fc))
         (/ (* c fa fb)
            (- fc fa)
            (- fc fb))) ; inverse quadratic interpolation
      (- b
         (/ (* fb (- b a))
            (- fb fa))))) ; secant interpolation


(define (pick-next-point a fa b fb c fc d tolerance previous-bisect)
  (let ([s (root-interpolate a fa b fb c fc)])
    (if (or ; A complex test to see if you really wanted a bisect step.
          (not (between? (/ (+ (* 3.0 a) b) 4.0) s b))
          (and previous-bisect
               (>= (abs (- s b)) (/ (abs (- b c)) 2.0)))
          (and (not previous-bisect)
               (>= (abs (- s b)) (/ (abs (- c d)) 2.0)))
          (and previous-bisect
               (< (abs (- b c)) tolerance))
          (and (not previous-bisect)
               (< (abs (- c d)) tolerance)))
        (values (/ (+ a b) 2.0) #t)
        (values s #f))))


(define (find-root-bracketing-interval a fa b fb s fs)
  (if (sign-difference? fa fs)
    (sort-interval a fa s fs)
    (sort-interval s fs b fb)))


(define* (brent-rootfind f a b #:optional #:key
                                          (tolerance 1e-10)
                                          (iterations 100))
  (define (converged? a b fa fb)
    (or (near? 0 fa)
        (near? 0 fb)
        (< (abs (- b a)) tolerance)))

  ; Sort the interval, in case it was given reversed.
  (receive (a fa b fb) (sort-interval a (f a) b (f b))
    (let loop ([a a]
               [fa fa]
               [b b]
               [fb fb]
               [c a]
               [fc fa]
               [d #f]
               [previous-bisect #t]
               [iterations (1- iterations)])
      (receive (s previous-bisect)
        (pick-next-point a fa b fb c fc d tolerance previous-bisect)
        (let ([fs (f s)]
              [d c]
              [c b])
          (receive (a fa b fb)
            (find-root-bracketing-interval a fa b fb s fs)
            (if (or (converged? a fa b fb)
                    (<= iterations 0))
                b
                (loop a fa b fb c fc d previous-bisect (1- iterations)))))))))


;------------------------------------------------------------------------------;
; The root should be about 0.45158270528.
(define (f x)
  (sin (cos (exp x))))

(let* ([x (brent-rootfind f 0 1)]
       [fx (f x)])
  (format #t "x, f(x): ~s, ~s\n" x fx))
