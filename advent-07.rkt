#lang racket

(require "advent07-data.rkt")

(define (simple-distance p1 p2)  
    (if (< p1 p2)
        (- p2 p1)
        (- p1 p2)))

(define (sum-of-distance-from x ps df)
  (for/sum ([p (in-list ps)])
    (df x p)))

(define (max ps)
  (for/fold ([acc 0])
            ([p (in-list ps)])
    (if (> p acc)
        p
        acc)))

(define (min ps)
  (for/fold ([acc (car ps)])
            ([p (in-list ps)])
    (if (< p acc)
        p
        acc)))

(define (min-distance df ps)
  (let ([min-p (min ps)]
        [max-p (max ps)])
    (for/fold ([min-sum (sum-of-distance-from min-p ps df)]
               [min-point min-p]
               #:result (values min-sum min-point))
              ([p (in-inclusive-range min-p max-p)])
      (let ([sum (sum-of-distance-from p ps df)])
        (if (< sum min-sum)
            (values sum p)
            (values min-sum min-point))))))

(define (simple-min-distance ps)
  (min-distance simple-distance ps))

(simple-min-distance test-data)
(simple-min-distance data)