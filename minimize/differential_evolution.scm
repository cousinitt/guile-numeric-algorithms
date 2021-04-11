(define-module (de)
  #:use-module (ice-9 optarg)
  #:export (make-de-optimizer))
;; NOTE - this likely doesn't work, I haven't touched it in months.
;; I do plan on fixing it up though.
 

(define (make-de-optimizer
         cost-function         ; must take vectors
         upper-limits          ; vector of lower parameter limits
         lower-limits          ; vector of upper parameter limits
         [population-size 500] ; size of the randomly-initialized pool
         [mutation-factor 0.8]
         [crossover-probability 0.5])
  
    (define iterations 0)
    (define minimum-cost inf)
    (define best-index 0)

    (when (not (eq? (vector-length upper)
                    (vector-length lower)))
        (error "upper and lower parameter vectors must be the same size"))

    ; Initialize the population vector to random samples between upper and lower.
    (define population
        (let ([p (make-vector population-size)])
            (define (initialize-vector idx)
                (let ([v (map random-between lower-limits upper-limits)])
                    (vector-set! p idx v)))
  
            (let loop ([s 0])
                (when (< s population-size)
                    (initialize-vector s)
                    (loop (1+ s)))))

    ; Initialize the costs to the value of the cost function for the
    ; parameters given above.
    (define costs
        (let ([c (make-vector population-size)])
            (let loop ([idx 0])
                (when (< idx population-size)
                    (vector-set! costs idx (cost (vector-ref population idx)))
                    (loop (1+ idx))))))

    (define (donor-vector idx)
        "This is the mutation step of the algorithm."
        ; If the parameters of the donor vector are outside of the range of the
        ; parameters, replace the entry with a randomly selected parameter in the
        ; correct range.
        (define (adjust-parameters! vec)
            (let loop ([idx 0])
                (let ([top (vector-ref upper-limits idx)]
                      [bottom (vector-ref lower-limits idx)]
                      [p (vector-ref vec idx)])
                    (when (< idx parameter-size)
                        (when (not (and (< p top) (> p bottom)))
                            (vector-set! vec idx (random-between bottom top)))
                        (loop (1+ idx))))))

        ; Find the donor vector for the given index.
        (let* ([inds (sample-without-replacement 3 population-size idx)]
               [samples (list->vector
                         (map (lambda (idx)
                                (vector-ref population idx)) inds))]
               [donor (make-vector parameter-size)])
            (adjust-parameters!
                (vector-add
                    (vector-ref samples 0)
                    (vector-scalar-mul mutation-factor
                        (vector-sub (vector-ref samples 1)
                                    (vector-ref samples 2)))))))


    (define (recombine idx)
        ; Inner function for random crossover.
        (define (maybe-set! idx original donor)
            (when (< (random) crossover-probability)
                (vector-set! original idx (vector-ref donor idx))))

        (let ([original (vector-ref population idx)]
              [donor (donor-vector idx)])
            (let loop ([n 0])
                (when (< n 3)
                    (maybe-set! n original donor)
                    (loop (1+ n))))
            original))


    (define (evolve idx)
        (let ([original (vector-ref population idx)]
              [evolved (recombine idx)])
            (let ([evolved-cost (cost evolved)]
                  [original-cost (cost original)])
                (when (> (cost evolved) (cost original))
                    (vector-set! population idx evolved))
                (when (< cost minimum-cost)
                    (set! minimum-cost cost)))))


;--------------------;
; Vector math stuff. ;
;--------------------;
(define (vector-add va vb)
    (list->vector (+ (vector->list va) (vector->list vb))))

(define (vector-sub va vb)
    (list->vector (- (vector->list va) (vector->list vb))))

(define (vector-scalar-mul S va)
    (list->vector (map (lambda (x) (* S x)) (vector->list va))))
    

;-------------------------;
; Random number utilities ;
;-------------------------;
(define (random-between a b)
    (let ([A (min a b)]
          [B (max a b)]
          [x (random)])
        (+ (* (- B A) x) A)))

;; https://stackoverflow.com/questions/311703
(define (sample-without-replacement n N xclude)
    (let loop ([num-seen 0]
               [num-stored 0]
               [result '()])
        (let ([u (random)])
            (cond [(>= num-stored n) result]
                  [(or (>= (* u (- N num-seen)) (- n num-stored))
                       (eq? num-seen xclude))
                   (loop (1+ num-seen) num-stored result)]
                  [else
                   (loop (1+ num-seen)
                         (1+ num-stored)
                         (cons num-seen result))]))))
