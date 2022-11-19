#lang racket

(require "advent06-data.rkt")

(define (step xs)
  (for/fold ([acc '()]
             [new '()]
             #:result (append (reverse acc) new))
            ([x xs])
    (if (= x 0)
      (values  (cons 6 acc) (cons 8 new))
      (values (cons (- x 1) acc) new))))

(define (run xs n)
  (for/fold ([fish xs]
             #:result (length fish))
            ([i (in-range n)])
    (step fish)))
            