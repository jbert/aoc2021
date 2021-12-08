#lang racket
(require "aoc.rkt")

(aoc-set-day 8)
(aoc-set-test #t)

(define lines (aoc-get-lines))

(define (parse-line l)
  (let* ([bits (string-split l " | ")]
         [patterns (string-split (first bits) " ")]
         [output (string-split (second bits) " ")])
    (list patterns output)))
(define entries (map parse-line lines))

(define (is-1478? seq)
  (let ([l (string-length seq)])
    (or (= l 2) ; 1
        (= l 4) ; 4
        (= l 3) ; 7
        (= l 7)))) ; 8

(printf "Number of 1478 output seqs: ~a\n"
        (let ([outputs (flatten (map second entries))])
          (length (filter is-1478? outputs))))