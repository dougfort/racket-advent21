#lang racket

(require "advent01-data.rkt")

(define test-depths (map string->number (string-split test-data)))
(define depths (map string->number (string-split raw-data)))

(define (increase-count measurements)
  (for/fold ([acc 0]
             [prev (car measurements)]
             #:result acc)
            ([measurement (cdr measurements)])
    (if (> measurement prev)
        (values (add1 acc) measurement)
        (values acc measurement))))

(define (window-sums measurements)
  (define (sum l) (foldr + 0 l))  
  (for/fold ([acc '()]
             [window (reverse (take measurements 3))]
             #:result (reverse (cons (sum window) acc)))
            ([measurement (drop measurements 3)])
    (values (cons (sum window) acc) (cons measurement (take window 2)))))

(increase-count test-depths)
(increase-count depths)

(increase-count (window-sums test-depths))
(increase-count (window-sums depths))
