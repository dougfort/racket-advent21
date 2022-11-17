#lang racket

(require "advent04-data.rkt" "advent04-board.rkt")

(define (find-winning-board board-data numbers)
  (let ([boards (map make-board board-data)])
    (for/or ([number (in-list numbers)])
      (for/or ([board (in-list boards)])
        (mark-board board number)))))
