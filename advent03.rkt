#lang racket

(require "advent03-data.rkt")

(define (scan width data)
  (for/fold ([count 0]
             [acc (make-vector width)]
             #:result (values count acc))
            ([s (string-split data)])
    
    (for/fold ([i 0]) ([ch (string->list s)])
      (when (equal? ch #\1)
        (vector-set! acc i (add1 (vector-ref acc i))))
      (add1 i))
          
    (values (add1 count) acc)))

(define (measurements count acc)
  (let ([half (/ count 2)])
    (for/fold ([gamma '()]
               [epsilon '()]
               #:result (values (list->string (reverse gamma)) (list->string (reverse epsilon))))
              ([measurement (vector->list acc)])
      (if (> measurement half)
          (values (cons #\1 gamma) (cons #\0 epsilon))
          (values (cons #\0 gamma) (cons #\1 epsilon))))))

(define (measure width data)
  (let-values ([(gamma epsilon) (call-with-values (λ () (scan width data)) measurements)])
    (let ([g (string->number gamma 2)]
          [e (string->number epsilon 2)])
      (printf "gamma = ~s; epsilon = ~s; product = ~s\n" g e (* g e)))))

(measure 5 test-data)
(measure 12 raw-data)

(define (most-common-bit pos data)
  (for/fold ([zeros 0]
             [ones 0]
             #:result (if (> zeros ones)
                          #\0
                          #\1))
            ([s (string-split data)])
    (if (equal? #\1 (string-ref s pos))
        (values zeros (add1 ones))
        (values (add1 zeros) ones))))

(define (extract mcb pos data)
  (for/list ([s (in-list (string-split data))]
        #:when (equal? (string-ref s pos) mcb))
    s))
    
    