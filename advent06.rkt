#lang racket

(require "advent06-data.rkt")

(define (step-list xs)
  (for/fold ([acc '()]
             [new '()]
             #:result (append (reverse acc) new))
            ([x xs])
    (if (= x 0)
      (values  (cons 6 acc) (cons 8 new))
      (values (cons (- x 1) acc) new))))

(define (run-list xs n)
  (for/fold ([fish xs]
             #:result (length fish))
            ([i (in-range n)])
    (step-list fish)))

(printf "test-data 80 days: ~s\n" (run-list test-data 80))
(printf "data 80 days: ~s\n" (run-list data 80))

(define (frame-update! frame i v)
  (vector-set! frame i (+ (vector-ref frame i) v))
  frame)

(define (load-frame data)
  (let ([frame (make-vector 9 0)])
    (for ([t (in-list data)])
      (frame-update! frame t 1))
    frame))

(define (step-frame prev)
  (let ([next (make-vector 9 0)])
    (for ([i (in-inclusive-range 0 8)])
      (if (= i 0)
          (begin
            (frame-update! next 6 (vector-ref prev i))  
            (frame-update! next 8 (vector-ref prev i)))
          (frame-update! next (- i 1) (vector-ref prev i))))
    next))

(define (run-frame xs n)
  (for/fold ([f xs]
             #:result (for/sum ([x (in-vector f)]) x))
            ([i (in-range n)])
    (step-frame f)))

(printf "test-data 256 days: ~s\n" (run-frame (load-frame test-data) 256))
(printf "data 256 days: ~s\n" (run-frame (load-frame data) 256))
