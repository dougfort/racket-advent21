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

(define (val-with-neighbors hm lp)
  (for/list ([np (heightmap-neighbors hm (value-at-point-p lp))])
    (cons (value-at-point-val lp) np)))

(define (find-basin hm lp)
  (let ([seen (make-hash)])
    (let loop ([stack (val-with-neighbors hm lp)]
               [acc (list lp)])
      (cond
        [(empty? stack) acc]
        [else
         (let ([val (caar stack)]
               [p (cdar stack)]
               [r (cdr stack)])
           (cond
             [(hash-has-key? seen (value-at-point-p p)) (loop r acc)]
             [(= (value-at-point-val p) 9) (loop r acc)]
             [(<= (value-at-point-val p) val) (loop r acc)]
             [else
              (hash-set! seen (value-at-point-p p) #t)
              (loop (append r (val-with-neighbors hm p)) (cons p acc))]))]))))

(define (largest-basins-product hm)
  (let ([bs (for/list ([lp (low-points hm)])
              (length (find-basin hm lp)))])
    (for/product ([i (take (reverse (sort bs <)) 3)])
      i)))

(largest-basins-product test-data-heightmap)
(largest-basins-product data-heightmap)