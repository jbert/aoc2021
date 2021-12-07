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

; 1 -> 1, 2 -> 1+2, 3 -> 1+2+3
(define (p2-crab-fuel-to n crab)
  (let ([d (abs (- n crab))])
    (/ (* d (add1 d))
       2)))

(define (p2-crabs-fuel-to n crabs)
  (apply + (map (lambda (c) (p2-crab-fuel-to n c))
                crabs)))

(define (p2-min-fuel crabs)
  (let ([top (length crabs)])
    (apply min (map (lambda (n) (p2-crabs-fuel-to n crabs))
                    (stream->list (in-range top))))))

(printf "P2 Min fuel spent is ~a\n" (p2-min-fuel crabs))
