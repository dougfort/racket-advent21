#lang racket

(provide make-board make-matches mark-board)

(define (board-index r c)
  (+ c (* r 5)))

; expect a list of 5 lists of numbers
(define (make-board data)
  (let ([board (make-vector 25)])
    (for ([r (in-naturals)]
          [row (in-list data)] )
      (for ([c (in-naturals)]
            [col (in-list row)])
        (vector-set! board (board-index r c) col)))          
    board))

(define (make-matches)
  (make-vector 25 #f))

(define (mark-board board matches number)
  (for/or ([i (in-naturals)]
           [n (in-vector board)])
    (if (equal? n number)
        (begin
          (vector-set! matches i #t)
          (if (or (marked-row matches (row i)) (marked-col matches (col i)))
              (compute-score board matches number)
              #f))
        #f)))

(define (row i)
  (quotient i 5))

(define (col i)
  (remainder i 5)) 

(define (marked-row matches r)
  (for/and ([c (in-range 5)])
    (vector-ref matches (board-index r c))))

(define (marked-col matches c)
  (for/and ([r (in-range 5)])
    (vector-ref matches (board-index r c))))

(define (compute-score board matches number)
  (let ([s (for*/sum ([r (in-range 5)]
                     [c (in-range 5)]
                     #:when (not (vector-ref matches (board-index r c))))
             (vector-ref board (board-index r c)))])
    (* number s)))