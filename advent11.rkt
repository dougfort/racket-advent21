#lang racket

(require "advent11-data.rkt")

(struct point (r c)
  #:transparent)

(define (make-point n)
  (point (quotient n height) (remainder n width)))

(define (is-valid? p)
  (and
   (>= (point-r p) 0)
   (< (point-r p) height)
   (>= (point-c p) 0)
   (< (point-c p) width)))

(define (neighbors p)
  (let ([raw-neighbors (list
                        (point (- (point-r p) 1) (- (point-c p) 1))
                        (point (- (point-r p) 1) (point-c p))
                        (point (- (point-r p) 1) (+ (point-c p) 1))
                        (point (point-r p) (- (point-c p) 1))
                        (point (point-r p) (+ (point-c p) 1))
                        (point (+ (point-r p) 1) (- (point-c p) 1))
                        (point (+ (point-r p) 1) (point-c p))
                        (point (+ (point-r p) 1) (+ (point-c p) 1)))])
    (filter (λ (p) (is-valid? p)) raw-neighbors)))

(define (show vec)
  (for/fold ([acc null]
             [v vec]
             #:result (reverse acc))
            ([r (in-range height)])
    (let-values ([(h t) (vector-split-at v width)])      
      (values (cons h acc) t))))

(define (index p)
  (+ (* (point-r p) width) (point-c p)))

(define (increment! vec p)
  (let ([i (index p)])
    (vector-set! vec i (add1 (vector-ref vec i)))))

(define (set-to-zero! vec p)
  (let ([i (index p)])
    (vector-set! vec i 0)))  

(define (flashable flashed v)
  (for/fold ([acc null])
            ([i (in-naturals)]
             [e (in-vector v)])
    (let ([p (make-point i)])
      (if (and (>= e 10) (not (hash-has-key? flashed p)))
          (cons p acc)
          acc))))

(define (flash v fp)
  (increment! v fp)
  (for ([np (neighbors fp)])
    (increment! v np))
  v)   

(define (step init-v)
  (let ([flashed (make-hash)])
    (let* ([v1 (vector-map add1 init-v)]
           [fps1 (flashable flashed v1)])
      (let ([v-final (let loop ([v v1]
                                [fps fps1])
                       (cond
                         [(empty? fps) v]
                         [else
                          (let ([fp (car fps)])
                            (hash-set! flashed fp #t)
                            (let* ([v2 (flash v fp)]
                                   [fps2 (flashable flashed v2)])
                              (loop  v2 fps2)))])
                       )])
        (for ([p (in-list (hash-keys flashed))])
          (set-to-zero! v-final p))

        (values (length (hash-keys flashed)) v-final)))))

(define (steps init-v n)
  (let loop ([count 0]
             [v init-v]
             [i 0])
    (cond
      [(= n i) (values count v)]
      [else
       (let-values ([(c v) (step v)])        
       (loop (+ count c) v (add1 i)))])))