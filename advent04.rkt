#lang racket

(require "advent04-data.rkt" "advent04-board.rkt")

(define (find-winning-board board-data numbers)
  (let ([boards (map make-board board-data)]
        [completed-boards (make-hash)])
    (for ([number (in-list numbers)])
      (for ([i (in-naturals)]
            [board (in-list boards)]
            #:unless (hash-has-key? completed-boards i))
        (let ([score (mark-board board number)])
          (if score
              (begin
                (printf "board ~s: score ~s\n" i score)
                (hash-set! completed-boards i #t))
              #f))))))
                                    
