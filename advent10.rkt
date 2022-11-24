#lang racket

(require "advent10-data.rkt")

(define chunk-bounds #hash((#\( . #\)) (#\[ . #\])  (#\{ . #\}) (#\< . #\>)))
(define defect-scores #hash((#\) . 3) (#\] . 57)  (#\} . 1197) (#\> .  25137)))
(define completion-scores #hash((#\) . 1) (#\] . 2)  (#\} . 3) (#\> .  4)))

; return a pair of the completion stack with the first incorrect closing character 
(define (scan l)
  (let loop ([chs (string->list l)]
             [stack '()])
    (cond
      [(empty? chs) (cons stack #f)]
      [else
       (let* ([ch (car chs)]
              [closer (hash-ref chunk-bounds ch #f)])
         (cond
           [closer (loop (cdr chs) (cons closer stack))]
           [else
            (if (equal? ch (car stack))
                (loop (cdr chs) (cdr stack))
                (cons stack ch))]))])))

(define (scan-all d)
  (for/fold ([acc null]
             #:result (reverse acc))            
             ([s (in-list d)])
    (let ([err (scan s)])
      (if err
          (cons err acc)
          acc))))

(define (sum-defect-scores xs)
  (for/sum ([x (in-list xs)])
    (hash-ref defect-scores (cdr x) 0)))

(sum-defect-scores (scan-all test-data))
(sum-defect-scores (scan-all data))

(define (compute-completion-score x)
  (for/fold ([score 0])
            ([cl (in-list (car x))])
    (+ (* score 5) (hash-ref completion-scores cl 0))))
             
(define (list-completion-scores xs)
  (for/list ([x (in-list xs)]
             #:when (not (cdr x)))
    (compute-completion-score x)))

(define (select-completion-score cs)
  (list-ref cs (quotient (length cs) 2)))

(select-completion-score (sort (list-completion-scores (scan-all test-data)) <))
(select-completion-score (sort (list-completion-scores (scan-all data)) <))