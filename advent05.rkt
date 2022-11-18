#lang racket

(require "advent05-data.rkt")

(define (count-overlap data)
  (let ([ch (make-hash)])
    (for ([l data])      
      (for-each (λ (p)
                  (hash-update! ch p add1 0))
                (points-in-line l)))
    (length (filter (λ (v) (> v 1)) (hash-values ch)))))
      
(count-overlap restricted-test-data)
(count-overlap restricted-data)