#lang racket
(require "aoc.rkt")

(aoc-set-day 3)
(aoc-set-test #f)

(define lines (aoc-get-lines))
; a bnum is a list of integers, all 0/1
(define (string->bnum s)
  (map (lambda (c) (if (equal? c #\0) 0 1))
         (string->list s)))
(define bnums (map string->bnum lines))
(define numbits (length (first bnums)))


(define (bnum-invert bnum)
  (map (lambda (bit) (if (= bit 0) 1 0)) bnum))

(define zero-count (make-list numbits 0))

(define (bnum-count c b)
  (for/list ([x c]
             [y b])
    (+ x y)))

(define ocounts (foldl bnum-count zero-count bnums))
(define zcounts (foldl bnum-count zero-count (map bnum-invert bnums)))

(define gamma
  (for/list ([zc zcounts]
             [oc ocounts])
    (if (< zc oc) 1 0)))

(define (rbnum->int rbnum)
  (if (= (length rbnum) 0)
      0
      (+ (first rbnum)
         (* 2
            (rbnum->int (rest rbnum))))))
(define (bnum->int bnum)
  (rbnum->int (reverse bnum)))

(define epsilon (bnum-invert gamma))

(define gamma-int (bnum->int gamma))
(define epsilon-int (bnum->int epsilon))
(printf "Epsilon: ~a Gamma: ~a Power consumption: ~a\n"
        epsilon-int gamma-int (* epsilon-int gamma-int))
        
; part 2

(define (filter-bnums-helper bnums pos excess-ones-to-wanted-bit)
  (if (= 1 (length bnums))
      bnums
      (let* ([ocounts  (foldl bnum-count zero-count bnums)]
             [zcounts  (foldl bnum-count zero-count (map bnum-invert bnums))]
             [excess-ones (- (list-ref ocounts pos) (list-ref zcounts pos))]
             [wanted-bit (excess-ones-to-wanted-bit excess-ones)])
        (when (> pos (length (first bnums)))
          (error "pos overflow length"))
        (filter-bnums-helper
         (filter (lambda (bnum) (= (list-ref bnum pos) wanted-bit))
                 bnums)
         (add1 pos)
         excess-ones-to-wanted-bit))))

(define (oxy-pred excess-ones)
  (if (>= excess-ones 0) 1 0))
  
(define (co2-pred excess-ones)
  (if (>= excess-ones 0) 0 1))
  
(define (filter-bnums bnums pred)
  (filter-bnums-helper bnums 0 pred))

(let ([oxy (first (filter-bnums bnums oxy-pred))]
      [co2 (first (filter-bnums bnums co2-pred))])
  (printf "oxygen generator:  ~a ~a\n" oxy (bnum->int oxy))
  (printf "co2 scrubber:  ~a ~a\n" co2 (bnum->int co2))
  (printf "life support rating: ~a" (* (bnum->int oxy) (bnum->int co2))))