#lang racket
(require "aoc.rkt")
(provide paper-fold
         paper-fold-horiz-line?
         paper-fold-value
         paper-fold-apply
         p)

(aoc-set-day 13)
(aoc-set-test #f)


(define lines (aoc-get-lines))
(define-values (coord-lines fold-lines) (splitf-at lines (lambda (l) (not (equal? "" l)))))
(define (parse-coord-line l)
  (let ([bits (map string->number (string-split l ","))])
    (p (first bits) (second bits))))
(define coords (map parse-coord-line coord-lines))
(define width (apply max (map p-x coords)))
(define height (apply max (map p-y coords)))

;(define zero-grid (grid-make width height))
;(define the-grid (foldl grid-inc zero-grid (list (p 1 2) (p 3 4))))

(struct paper-fold (horiz-line? value) #:transparent)
(define (parse-fold-line l)
  (let* ([bits (string-split l "=")]
         [h? (equal? (first bits) "fold along y")]
         [value (string->number (second bits))])
    (paper-fold h? value)))
(define paper-folds (map parse-fold-line (cdr fold-lines)))

(define (paper-fold-apply pf pt)
  (let ([v (paper-fold-value pf)])
    (if (paper-fold-horiz-line? pf)
        (let ([dy (- (p-y pt) v)])
          (if (> dy 0)
              (p (p-x pt)
                 (- v dy))
              pt))
        (let ([dx (- (p-x pt) v)])
          (printf "dx ~a\n" dx)
          (if (> dx 0)
              (p (- v dx)
                 (p-y pt))
              pt)))))

(define (apply-fold-to-coords-set pf cs)
  (list->set (set-map cs (lambda (pt) (paper-fold-apply pf pt)))))

;coords
;paper-folds
;(apply-fold-to-coords-set (second paper-folds) coords)
;(apply-fold-to-coords-set (second paper-folds) (apply-fold-to-coords-set (first paper-folds) coords))
(printf "Part 1: ~a\n" (set-count (apply-fold-to-coords-set (first paper-folds) coords)))
