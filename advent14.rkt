#lang racket

(require "advent14-data.rkt")

(define (step rules t)
  (let ([tmpl (string->list t)])
    (for/fold ([acc (list (car tmpl))]
               [prev (car tmpl)]
               #:result (list->string (reverse acc)))
              ([next (cdr tmpl)])
      (values (cons next (cons (hash-ref rules (cons prev next)) acc)) next))))

(define (frequency t)
  (let ([tmpl (string->list t)]
        [db (make-hash)])
    (for ([ch (in-list tmpl)])
      (hash-update! db ch add1 0))
    db)) 

(define (steps n rules t)
  (let loop ([i 1]
             [tnext (step rules t)])
    (cond
      [(= i n) tnext]
      [else (loop (add1 i) (step rules tnext))])))

(define (part1 rules t)
   (let* ([end-tmpl (steps 10 rules t)]
          [freq-hash (frequency end-tmpl)]
          [min (apply min (hash-values freq-hash))]
          [max (apply max (hash-values freq-hash))])
     (- max min)))

(part1 test-pair-insertion-rules test-polymer-template)         
(part1 pair-insertion-rules polymer-template)         
