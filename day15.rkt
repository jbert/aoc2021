#lang racket

(require "aoc.rkt")
(require "grid.rkt")

(aoc-set-day 15)
(aoc-set-test #t)

(define lines (aoc-get-lines))
(define rows (map (lambda (l) (map string->number
                                   (map string (string->list l))))
                    lines))

(define (a_star start goal h)