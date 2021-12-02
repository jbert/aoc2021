#lang racket
(require "aoc.rkt")

(aoc-set-day 1)
(aoc-set-test #f)

(define nums (aoc-get-nums))

(define deltas (for/list ([a nums]
                          [b (rest nums)])
                 (- a b)))
(displayln (format "Number of increases: ~s"
                   (length (filter negative? deltas))))
;(for ([num (map string->number lines)])
;  (displayln num))

(define window-sums (for/list ([a nums]
                               [b (rest nums)]
                               [c (rest (rest nums))])
                      (+ a b c)))
(define window-deltas (for/list ([a window-sums]
                                 [b (rest window-sums)])
                        (- a b)))
(displayln (format "Number of window increases: ~s"
                   (length (filter negative? window-deltas))))
