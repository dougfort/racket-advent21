#lang racket

(require "advent13-data.rkt")

(struct dots (w h v)
  #:transparent)

(define (find-max ps)
  (for/fold ([max-x 0]
             [max-y 0]
             #:result (values max-x max-y))
            ([p (in-list ps)])
    (values (max max-x (first p)) (max max-y (second p)))))
            
(define (load-dots ps)
  (let-values ([(max-x max-y) (find-max ps)])
    (let* ([w (add1 max-x)]
           [h (add1 max-y)]
           [vec (make-vector (* w h) #f)])
      (for ([p (in-list ps)])
        (let ([x (first p)]
              [y (second p)])
          (vector-set! vec (+ (* y w) x) #t)))
      (dots w h vec))))

(define (show-dots dts)
  (printf "\n")
  (for ([p (vector->list (dots-v dts))]
        [i (in-naturals)])
    (when (zero? (remainder i (dots-w dts))) 
      (printf "\n" ))
    (if p
        (printf "#")
        (printf ".")))
  (printf "\n" ))

(define (count-dots dts)
  (vector-count (λ (x) x) (dots-v dts)))

(define (fold-dots-up dts y)
  (let* ([w (dots-w dts)]
         [h (quotient (dots-h dts) 2)]
         [vec (make-vector (* w h) #f)])
    (for ([i (in-range y)]
          [j (in-range (sub1 (dots-h dts)) y -1)])
      (let ([offset-i (* i (dots-w dts))]
            [offset-j (* j (dots-w dts))])
        (for ([x (in-range (dots-w dts))])
          (let* ([pos-i (+ offset-i x)]
                 [pos-j (+ offset-j x)]
                 [val (or (vector-ref (dots-v dts) pos-i) (vector-ref (dots-v dts) pos-j))])
            (vector-set! vec pos-i val)))))
    (dots w h vec)))
              
(define (fold-dots-left dts x)
  (let* ([w (quotient (dots-w dts) 2)]
         [h (dots-h dts)]
         [vec (make-vector (* w h) #f)])
    (for ([y (in-range (dots-h dts))])
      (let ([in-offset (* y (dots-w dts))]
            [out-offset (* y w)])
        (for ([i (in-range x)]
              [j (in-range (sub1 (dots-w dts)) x -1)])
          (let* ([pos-i (+ in-offset i)]
                 [pos-j (+ in-offset j)]
                 [pos-out (+ out-offset i)]
                 [val (or (vector-ref (dots-v dts) pos-i) (vector-ref (dots-v dts) pos-j))])
            (vector-set! vec pos-out val)))))
    (dots w h vec)))