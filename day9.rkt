#lang racket
(require "aoc.rkt")

(aoc-set-day 9)
(aoc-set-test #f)

(define lines (aoc-get-lines))

(define cells
  (map (lambda (l) 
         (map (lambda (c) (string->number (string c)))
              (string->list l)))
       lines))

(define g (grid-new cells))

(define low-points
  (filter (lambda (v) v)
          (flatten
           (grid-for-each
            g
            (lambda (v p)
              (let ([neighbour-min (apply min (grid-neighbour-get g p))])
                (if (< v neighbour-min)
                    p
                    #f)))))))

(define (risk-level g p)
  (add1 (grid-get g p)))

(printf "Sum risk levels ~a\n" (apply +
                                      (map (lambda (p) (risk-level g p))
                                           low-points)))
