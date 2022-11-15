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
            ([s data])
    (if (equal? #\1 (string-ref s pos))
        (values zeros (add1 ones))
        (values (add1 zeros) ones))))

(define (least-common-bit pos data)
  (let ([mcb (most-common-bit pos data)])
    (if (equal? mcb #\1)
        #\0
        #\1)))

(define (extract mcb pos data)
  (for/list ([s (in-list data)]
             #:when (equal? (string-ref s pos) mcb))
    s))

(define (ox-gen-rating width raw-data)
  (for/fold ([residue (string-split raw-data)]
             #:result (car residue))
            ([i (range 0 width)])
    (if (equal? 1 (length residue))
        residue
        (let ([mcb (most-common-bit i residue)])
          (let ([residue (extract mcb i residue)])
            residue)))))

(define (c02-scrubber-rating width raw-data)
  (for/fold ([residue (string-split raw-data)]
             #:result (car residue))
            ([i (range 0 width)])
    (if (equal? 1 (length residue))
        residue
        (let ([lcb (least-common-bit i residue)])
          (let ([residue (extract lcb i residue)])
            residue)))))

(define (ratings width data)
  (let ([ox-gen (string->number (ox-gen-rating width data) 2)]
        [c02-scrubber (string->number (c02-scrubber-rating width data) 2)])
    (printf "ox gen rating = ~s; c02 scrubber rating = ~s; product = ~s\n"
            ox-gen c02-scrubber (* ox-gen c02-scrubber))))

(ratings 5 test-data)
(ratings 12 raw-data)

        
