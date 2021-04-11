#!/usr/bin/guile
!#
; This website was great for describing the flow of the algorithm:
; https://codetobuy.com/downloads/brent-dekker-method-as-a-root-finding-method-of-any-equation/

(use-modules (ice-9 receive))

; Variables ending in '0' refer to the previous step.
; Variables ending in '1' refer to this step.
; Variables ending in '2' refer to the next step.
; The current best guess is "b1".
; The "counterpoint" is a point with an opposite sign from the current
; guess - so that the two bracket the root.

(define (pick-next-point a1 fa1 b1 fb1 b2 fb2)
    (let* ([m (/ (+ a1 b1) 2.0)] ; midpoint
           [fm (f m)]
           [s (- b2
                 (/ (* fb2 (- b2 b1))
                    (- fb2 fb1)))] ; secant intercept
           [fs (f s)])
      (if (between? m s b1)
          (values s fs)
          (values m fm))))


(define (between? a x b)
  (let ([interval-size (abs (- b a))])
    (and (< (abs (- b x)) interval-size)
         (< (abs (- x a)) interval-size))))


(define (pick-next-counterpoint a1 fa1 b1 fb1 fb2)
  (if (sign-difference? fa1 fb2)
      (values a1 fa1)
      (values b1 fb1)))


(define (sign-difference? u v)
  (>= 0.0 (* u v)))


(define (sort-interval a2 fa2 b2 fb2)
  (if (< (abs fa2) (abs fb2))
      (values b2 fb2 a2 fa2)
      (values a2 fa2 b2 fb2)))


(define* (dekker-rootfind f a b #:optional #:key
                                           (tolerance 1e-10)
                                           (iterations 100))
  (define (converged? b1 b0)
    (< (abs (- b1 b0)) tolerance))

  (let ([fa (f a)]
        [fb (f b)])
    (receive (a fa b fb) (sort-interval a fa b fb) ; initialize
             (let loop ([a1 a]
                        [fa1 fa]
                        [b1 b]
                        [fb1 fb]
                        [b0 a]
                        [fb0 fa]
                        [iterations (1- iterations)])
               (if (or (converged? b1 b0)
                       (<= iterations 0))
                   b1
                   (receive (b2 fb2) (pick-next-point a1 fa1 b1 fb1 b0 fb0)
                     (receive (a2 fa2) (pick-next-counterpoint a1 fa1 b1 fb1 fb2)
                       (receive (a2 fa2 b2 fb2) (sort-interval a2 fa2 b2 fb2)
                         (loop a2 fa2 b2 fb2 a1 fa1 (1- iterations))))))))))


; should be about 0.45158270528
(define (f x)
  (sin (cos (exp x))))

(let* ([x (dekker-rootfind f 0 1)]
       [fx (f x)])
  (format #t "x, f(x): ~s, ~s\n" x fx))
