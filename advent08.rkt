#lang racket

(require "advent08-data.rkt")

(define (count-segments line)
  (let ([segs (car (cdr line))])
    (map string-length segs)))

(define (count-known-digits data)
  (for/sum ([line (in-list data)])
   (for/sum ([c (count-segments line)])
     (if (member c '(2 4 3 7))
         1
         0))))

(count-known-digits test-data)
(count-known-digits data)