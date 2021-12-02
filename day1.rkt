#lang racket
(define work-dir "/home/john/dev/jbert/aoc2021")
;(current-directory work-dir)

(define day 1)
;(define suffix "-test")
(define suffix "")

(define data-file (format "~a/data/day~a~a.txt" work-dir day suffix))
;(displayln data-file)
(define lines (file->lines data-file))
(define nums (map string->number lines))

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
