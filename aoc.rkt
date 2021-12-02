#lang racket
(provide
 aoc-get-lines
 aoc-get-nums
 aoc-set-day
 aoc-set-test)


(define day 0)
(define suffix "")

(define (aoc-set-day newday)
  (set! day newday))

(define (aoc-set-test p)
  (set! suffix (if p "-test" "")))

(define work-dir "/home/john/dev/jbert/aoc2021")

(define (aoc-get-lines)
  (let ([data-filename (format "~a/data/day~a~a.txt" work-dir day suffix)])
    (file->lines data-filename)))

(define (aoc-get-nums)
  (map string->number (aoc-get-lines)))
