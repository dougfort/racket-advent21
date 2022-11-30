#lang racket

(require "advent12-data.rkt")

(define (cave-names l)
  (remove-duplicates (flatten l)))

(define (load-adj-mat cn l)
  (let* ([cave-count (length cn)]
         [adj-mat (make-vector (* cave-count cave-count) #f)]
         [loc (λ (r c) (+ (* r cave-count) c))])
    (for ([conn (in-list l)])
      (let* ([i1 (index-of cn (first conn))]
             [i2 (index-of cn (second conn))]
             [loc1 (loc i1 i2)]
             [loc2 (loc i2 i1)])
        (vector-set! adj-mat loc1 #t)
        (vector-set! adj-mat loc2 #t)))
    adj-mat))

(define (show cn vec)
  (let ([cnt (length cn)])
    (for/fold ([acc null]
               [v vec]
               #:result (reverse acc))
              ([r (in-range cnt)])
      (let-values ([(h t) (vector-split-at v cnt)])      
        (values (cons h acc) t)))))

(define (show-row name cn vec)
  (let* ([i (index-of cn name)]
         [offset (* i (length cn))])
    (for/list ([j (in-range (length cn))]
               #:when (vector-ref vec (+ offset j)))
      (list-ref cn j))))

(define (get-row-names row-name cn adj-mat)
  (let*  ([i (index-of cn row-name)]
          [offset (* i (length cn))])
    (for/list ([j (in-range (length cn))]
               #:when (vector-ref adj-mat (+ offset j)))
      (list-ref cn j))))         

(define (find-paths l)
  (let* ([cn (cave-names l)]
         [adj-mat (load-adj-mat cn l)])
    (let loop ([nm "start"]
               [acc (list "start")])
      (let ([nms (get-row-names nm cn adj-mat)])
        (printf "nms: ~s; acc: ~s\n" nms acc)
        (cond
          [(empty? nms) acc]
          [(equal? (car nms) "end") (cons "end" acc)]
          [(equal? (car nms) "start") (loop (second nms) acc)]
          [else (loop (car nms) (cons (car nms) acc))])))))
      
