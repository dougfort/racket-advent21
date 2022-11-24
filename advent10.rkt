#lang racket

(require "advent10-data.rkt")

(define chunk-bounds #hash((#\( . #\)) (#\[ . #\])  (#\{ . #\}) (#\< . #\>)))
(define scores #hash((#\) . 3) (#\] . 57)  (#\} . 1197) (#\> .  25137)))

; return #f or the first incorrect closing character
(define (scan l)
  (let loop ([chs (string->list l)]
             [stack '()])
    (cond
      [(empty? chs) #f]
      [else
       (let* ([ch (car chs)]
              [closer (hash-ref chunk-bounds ch #f)])
         (cond
           [closer (loop (cdr chs) (cons closer stack))]
           [else
            (if (equal? ch (car stack))
                (loop (cdr chs) (cdr stack))
                ch)]))])))

(define (scan-all d)
  (for/fold ([acc null]
             #:result (reverse acc))            
             ([s (in-list d)])
    (let ([err (scan s)])
      (if err
          (cons err acc)
          acc))))

(define (compute-score xs)
  (for/sum ([x (in-list xs)])
    (hash-ref scores x 0)))
      
(compute-score (scan-all test-data))
(compute-score (scan-all data))  