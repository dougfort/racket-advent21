#lang racket

(struct cell (val marked))

(provide make-board mark-board)

(define (board-index r c)
  (+ c (* r 5)))

; expect a list of 5 lists of numbers
(define (make-board data)
  (let ([board (make-vector 25)])
    (for ([r (in-naturals)]
          [row (in-list data)] )
      (for ([c (in-naturals)]
            [val (in-list row)])
        (vector-set! board (board-index r c) (cell val #f))))          
    board))

(define (mark-board board number)
  (for/or ([i (in-naturals)]
           [b (in-vector board)])
    (if (equal? (cell-val b) number)
        (begin
          (vector-set! board i (cell (cell-val b) #t))
          (if (or (marked-row board (row i)) (marked-col board (col i)))
              (compute-score board number)
              #f))
        #f)))

(define (row i)
  (quotient i 5))

(define (col i)
  (remainder i 5)) 

(define (marked-row board r)
  (for/and ([c (in-range 5)])
    (let ([b (vector-ref board (board-index r c))])
      (if b
          (cell-marked b)
          #f))))

(define (marked-col board c)
  (for/and ([r (in-range 5)])
    (let ([b (vector-ref board (board-index r c))])
      (if b
          (cell-marked b)
          #f))))

(define (compute-score board number)
  (let ([s (for*/sum ([r (in-range 5)]
                      [c (in-range 5)])
             (let ([b (vector-ref board (board-index r c))])
               (if b
                   (if (not (cell-marked b))
                       (cell-val b)
                       0)
                   0)))])
    (* number s)))