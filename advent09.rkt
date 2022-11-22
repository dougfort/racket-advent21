#lang racket

(require "advent09-data.rkt")

(define (low-points hm)
  (for*/fold ([acc '()])
             ([r (in-range (heightmap-l hm))]
              [c (in-range (heightmap-w hm))])
    (let ([val (heightmap-ref hm r c)]
          [ns (heightmap-neighbors hm r c)])
      (let-values ([(lp ct) (for/fold ([lp val]
                                       [ct 0]
                                       #:result (values lp ct))
                                      ([n (in-list ns)])
                              (cond
                                [(< n lp) (values n 0)]
                                [(= n lp) (values lp (add1 ct))]
                                [else (values lp ct)]))])
        (if (and (= lp val) (zero? ct)) 
            (cons val acc)
            acc)))))
            
(define (sum-risk-levels hm)
  (for/sum ([lp (low-points hm)])
    (+ 1 lp)))
    
                
(sum-risk-levels test-data-heightmap)
(sum-risk-levels data-heightmap)