#lang racket
(require "aoc.rkt")

(aoc-set-day 7)
(aoc-set-test #f)

(define lines (aoc-get-lines))

(define crabs (map string->number (string-split (first lines) ",")))

(define (crabs-fuel-to n crabs)
  (apply + (map (lambda (c) (abs (- n c)))
                crabs)))

(define (p1-min-fuel crabs)
  (let ([top (length crabs)])
    (apply min (map (lambda (n) (crabs-fuel-to n crabs))
                    (stream->list (in-range top))))))

(printf "Min fuel spent is ~a\n" (p1-min-fuel crabs))