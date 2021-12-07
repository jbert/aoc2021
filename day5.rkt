#lang racket
(require "aoc.rkt")

(aoc-set-day 5)
(aoc-set-test #f)

(define lines (aoc-get-lines))


; ------------

(define the-line-segs (map ls-parse lines))

(define diagram-width
  (add1 (apply max (flatten
                    (map (lambda (ls) (list (p-x (ls-from ls))
                                            (p-x (ls-to ls))))
                         the-line-segs)))))
(define diagram-height
  (add1 (apply max (flatten
                    (map (lambda (ls) (list (p-y (ls-from ls))
                                            (p-y (ls-to ls))))
                         the-line-segs)))))

(define the-diagram (grid-make diagram-width diagram-height))


(define (diagram-apply-ls ls d)
  (let ([pts (ls-points ls)])
    (foldl grid-inc d pts)))

(define the-hv-line-segs
  (filter (lambda (ls) (or (ls-vert? ls) (ls-horiz? ls)))
          the-line-segs))

(define (diagram-apply-line-segs line-segs d)
  (foldl diagram-apply-ls d line-segs))

(define the-p1-diagram (diagram-apply-line-segs the-hv-line-segs the-diagram))
(printf "Number of at-least-2s: ~a\n"
        (grid-count the-p1-diagram (lambda (v) (>= v 2))))

; part2

(define the-p2-diagram (diagram-apply-line-segs the-line-segs the-diagram))
(printf "P2: Number of at-least-2s: ~a\n"
        (grid-count the-p2-diagram (lambda (v) (>= v 2))))
