#lang racket

(provide small-test-data medium-test-data large-test-data data)

(define (parse d)
  (for/list ([s (string-split (string-trim d))])
    (string-split s "-")))

(define raw-small-test-data
  "
start-A
start-b
A-c
A-b
b-d
A-end
b-end
")
(define small-test-data (parse raw-small-test-data))

(define raw-medium-test-data
  "
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")
(define medium-test-data (parse raw-medium-test-data))

(define raw-large-test-data
  "
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")
(define large-test-data (parse raw-large-test-data))

(define raw-data
  "
xq-XZ
zo-yr
CT-zo
yr-xq
yr-LD
xq-ra
np-zo
end-LD
np-LD
xq-kq
start-ra
np-kq
LO-end
start-xq
zo-ra
LO-np
XZ-start
zo-kq
LO-yr
kq-XZ
zo-LD
kq-ra
XZ-yr
LD-ws
np-end
kq-yr
")
(define data (parse raw-data))
