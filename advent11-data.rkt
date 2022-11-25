#lang racket

(provide width height test-data data)

(define width 10)
(define height 10)

(define (row-to-list s)
  (for/list ([ch (string->list (string-trim s))])
    (- (char->integer ch) (char->integer #\0))))

(define (text-to-vec t)
  (list->vector (flatten (for/list ([s (string-split (string-trim t))])
                           (row-to-list s)))))

(define raw-test-data
  "
5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
")
(define test-data (text-to-vec raw-test-data))

(define raw-data
  "
5433566276
6376253438
8458636316
6253254525
7211137138
1411526532
5788761424
8677841514
1622331631
5876712227
")
(define data (text-to-vec raw-data))
