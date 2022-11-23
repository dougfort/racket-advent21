#lang racket

(require "advent09-data.rkt")

(define (low-points hm)
  (for*/fold ([acc '()])
             ([r (in-range (heightmap-l hm))]
              [c (in-range (heightmap-w hm))])
    (let* ([p (point r c)] 
           [vp (value-at-point p (heightmap-ref hm p))]
           [ns (heightmap-neighbors hm p)])
      (let-values ([(lvp ct) (for/fold ([lvp vp]
                                        [ct 0]
                                        #:result (values lvp ct))
                                       ([n (in-list ns)])
                               (cond
                                 [(< (value-at-point-val n) (value-at-point-val lvp)) (values n 0)]
                                 [(= (value-at-point-val n) (value-at-point-val lvp)) (values lvp (add1 ct))]
                                 [else (values lvp ct)]))])
        (if (and (= (value-at-point-val lvp) (value-at-point-val vp)) (zero? ct)) 
            (cons lvp acc)
            acc)))))
            
(define (sum-risk-levels hm)
  (for/sum ([lp (low-points hm)])
    (+ 1 (value-at-point-val lp))))
    
                
(sum-risk-levels test-data-heightmap)
(sum-risk-levels data-heightmap)

(define (find-basin hm lp)
  (let ([seen (make-hash)])
    (let loop ([prev-p lp]
               [stack (heightmap-neighbors hm (value-at-point-p lp))]
               [acc (list lp)])
      (cond
        [(empty? stack) acc]
        [else
         (let ([p (car stack)]
               [r (cdr stack)])
           (cond
             [(= (value-at-point-val p) 9) (loop prev-p r acc)]
             [(<= (value-at-point-val p) (value-at-point-val prev-p)) (loop prev-p r acc)]
             [else
               (loop p (append r (heightmap-neighbors hm (value-at-point-p p))) (cons p acc))]))]))))
                