#lang racket

(provide test-polymer-template test-pair-insertion-rules polymer-template pair-insertion-rules)

(define (parse d)
  (let* ([raw-pair (string-split (string-trim d) "\n\n")]
         [poly-tmpl (first raw-pair)]
         [rules (parse-rules (second raw-pair))])
    (values poly-tmpl rules)))

(define (parse-rules rs)
  (for/hash ([s (string-split rs "\n")])
    (let* ([p (string-split s " -> ")]
           [key-list (string->list (first p))]
           [value-list (string->list (second p))])
      (values (cons (first key-list) (second key-list)) (car value-list))))) 

(define raw-test-data
  "
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
")
(define-values (test-polymer-template test-pair-insertion-rules) (parse raw-test-data))

(define raw-data
  "
SVCHKVFKCSHVFNBKKPOC

NC -> H
PK -> V
SO -> C
PH -> F
FP -> N
PN -> B
NP -> V
NK -> S
FV -> P
SB -> S
VN -> F
SC -> H
OB -> F
ON -> O
HN -> V
HC -> F
SN -> K
CB -> H
OP -> K
HP -> H
KS -> S
BC -> S
VB -> V
FC -> B
BH -> C
HH -> O
KH -> S
VF -> F
PF -> P
VV -> F
PP -> V
BO -> H
BF -> B
PS -> K
FO -> O
KF -> O
FN -> H
CK -> B
VP -> V
HK -> F
OV -> P
CS -> V
FF -> P
OH -> N
VS -> H
VO -> O
CP -> O
KC -> V
KV -> P
BK -> B
VK -> S
NF -> V
OO -> V
FH -> H
CN -> O
SP -> B
KN -> V
OF -> H
NV -> H
FK -> B
PV -> N
NB -> B
KK -> P
VH -> P
CC -> B
HV -> V
OC -> H
PO -> V
NO -> O
BP -> C
NH -> H
BN -> O
BV -> S
CV -> B
HS -> O
NN -> S
NS -> P
KB -> F
CO -> H
HO -> P
PB -> B
BS -> P
SH -> H
FS -> V
SF -> O
OK -> F
KP -> S
BB -> C
PC -> B
OS -> C
SV -> N
SK -> K
KO -> C
SS -> V
CF -> C
HB -> K
VC -> B
CH -> P
HF -> K
FB -> V
")
(define-values (polymer-template pair-insertion-rules) (parse raw-data))
