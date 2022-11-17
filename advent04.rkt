#lang racket

(require "advent04-data.rkt" "advent04-board.rkt")

(define (find-winning-board board-data numbers)
  (let* ([boards (map make-board board-data)]
         [matches-list (for/list ([_ (in-range (length boards))]) (make-matches))])
    (for/or ([number (in-list numbers)])
      (for/or ([board (in-list boards)]
               [matches (in-list matches-list)])
        (let ([value (mark-board board matches number)])
          (printf "value: ~s\n" value)
          (if value
              (begin
                (printf "value = ~s\n" value)
                value)
              #f))))))
