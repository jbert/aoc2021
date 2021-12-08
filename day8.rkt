#lang racket
(require "aoc.rkt")

(aoc-set-day 8)
(aoc-set-test #t)

(define lines (aoc-get-lines))

(define (parse-line l)
  (define (parse-seqs s)
    (map (lambda (char-seq)
           (map (lambda (c) (string->symbol (string c)))
                char-seq))
         (map string->list (string-split s " "))))
  (let* ([bits (string-split l " | ")])
    (list (parse-seqs (first bits))
          (parse-seqs (second bits)))))
(define entries (map parse-line lines))

(define (is-1478? seq)
  (let ([l (length seq)])
    (or (= l 2) ; 1
        (= l 4) ; 4
        (= l 3) ; 7
        (= l 7)))) ; 8

(printf "Number of 1478 output seqs: ~a\n"
        (let ([outputs (apply append (map second entries))])
          (length (filter is-1478? outputs))))


; A mapping is a permutation of '(a b c d e f g)