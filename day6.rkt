#lang racket
(require "aoc.rkt")

(aoc-set-day 6)
(aoc-set-test #f)

(define lines (aoc-get-lines))

(define fishes (map string->number (string-split (first lines) ",")))

; return a list with 1 or 2 fish in it
(define (fish-tick f)
  (if (= f 0)
      (list 6 8)
      (list (- f 1))))

(define (fishes-tick fs)
  (flatten (map fish-tick fs)))

(define num-days 80)

(define (fishes-iterate fs days)
  (if (= days 0)
      fs
      (fishes-iterate (fishes-tick fs) (sub1 days))))

(printf "After ~a days, ~a fishes\n"
        num-days
        (length (fishes-iterate fishes num-days)))

; part 2

(define p2-num-days 256)

; 9 possible values for a fish
(define (fish-count fs)
  (let ([zero-counts (make-list 9 0)])
    (foldl count-inc-foldl zero-counts fs)))

(define (fish-counts-tick fcounts)
  (let* ([spawners (first fcounts)]
         [new-counts (count-add (rest fcounts) 6 spawners)])
    (append new-counts (list spawners))))

(define (fish-counts-iterate fcounts days)
  (if (= days 0)
      fcounts
      (fish-counts-iterate (fish-counts-tick fcounts) (sub1 days))))

(printf "After ~a days, ~a fishes\n"
        p2-num-days
        (apply + (fish-counts-iterate (fish-count fishes) p2-num-days)))
