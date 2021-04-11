#! /usr/bin/guile
!#

(define* (fibonacci-minimize f a b #:optional #:key
                                              (tolerance 1e-10)
                                              (iterations 100))
  (define (initialize-fibonacci)
    ; I'm trying to find the window in the Fibonacci sequence where:
    ; Fm0 >= (b - a)/tolerance.
    ; Fm0 is F[m - 0]
    ; Fm1 is F[m - 1]
    ; ... etc ...
    (let ([s (/ (- b a) tolerance)])
      (let loop ([Fm0 1.0]
                 [Fm1 1.0]
                 [Fm2 0.0])
        (if (< Fm0 s)
            (loop (+ Fm0 Fm1) Fm0 Fm1)
            (list Fm0 Fm1 Fm2)))))


  (let* ([init (initialize-fibonacci)]
         [Fm0 (car init)]
         [Fm1 (cadr init)]
         [Fm2 (caddr init)]
         [x1i (* (/ Fm2 Fm0) (- b a))]
         [x2i (* (/ Fm1 Fm0) (- b a))])
    (let loop ([a a]
               [x1 x1i]
               [fx1 (f x1i)]
               [x2 x2i]
               [fx2 (f x2i)]
               [b b]
               [Fm0 Fm0]
               [Fm1 Fm1]
               [Fm2 Fm2]
               [iterations (1- iterations)])
      (if (and (> Fm1 0)
               (> iterations 0))
          (if (< fx1 fx2)
              (let* ([x2 x1]
                     [b  x2]
                     [fx2 fx1]
                     [Fm0 Fm1]
                     [Fm1 Fm2]
                     [Fm2 (- Fm0 Fm1)]
                     [x1 (+ a (* (/ Fm2 Fm0) (- b a)))]
                     [fx1 (f x1)])
                (loop a x1 fx1 x2 fx2 b Fm0 Fm1 Fm2 (1- iterations)))
              (let* ([a  x1]
                     [x1 x2]
                     [fx1 fx2]
                     [Fm0 Fm1]
                     [Fm1 Fm2]
                     [Fm2 (- Fm0 Fm1)]
                     [x2 (+ a (* (/ Fm1 Fm0) (- b a)))]
                     [fx2 (f x2)])
                (loop a x1 fx1 x2 fx2 b Fm0 Fm1 Fm2 (1- iterations))))
          (if (> fx1 fx2)
              (cons x2 fx2)
              (cons x1 fx1))))))


(define (f x)
  (let* ([w (- x 10.0)]
         [z (/ (* w w) 50)])
    (- (exp (- z)))))


(let ([res (fibonacci-minimize f 0 20)])
  (format #t "x, f(x): ~s, ~s\n" (car res) (cdr res)))
