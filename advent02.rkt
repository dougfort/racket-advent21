#lang racket

(require "advent02-data.rkt")

(define (move commands)
  (for/fold ([h 0]
             [d 0]
             #:result (values h d))
            ([command commands])
    (let-values ([(dir amount) (parse command)])
      (cond
        [(equal? dir "forward") (values (+ h amount) d)]
        [(equal? dir "up") (values h (- d amount))]
        [(equal? dir "down") (values h (+ d amount))]
        [else (error "unknown command")]))))

(define (final commands)
  (let-values ([(h d) (move commands)])
    (printf "horizontal: ~s; depth: ~s; product: ~s\n" h d (* h d))))

(define (move-with-aim commands)
  (for/fold ([a 0]
             [h 0]
             [d 0]
             #:result (values h d))
            ([command commands])
    (let-values ([(dir amount) (parse command)])
      (cond
        [(equal? dir "forward") (values a (+ h amount) (+ d (* a amount)))]
        [(equal? dir "up") (values (- a amount) h d)]
        [(equal? dir "down") (values (+ a amount) h d)]
        [else (error "unknown command")]))))

(define (final-with-aim commands)
  (let-values ([(h d) (move-with-aim commands)])
    (printf "with aim: horizontal: ~s; depth: ~s; product: ~s\n" h d (* h d))))

(final (string-split test-data "\n"))
(final (string-split raw-data "\n"))
(final-with-aim (string-split test-data "\n"))
(final-with-aim (string-split raw-data "\n"))