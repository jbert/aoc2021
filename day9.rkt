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


; Assume all basins bounded by 9s
; So don't need to implement basin-merging, each low point
; point will flood out to one basin
(define (get-basin g p)
  (define (helper last-pts seen)
;    (printf "\nlast-pts ~a seen ~a\n" last-pts seen)
    (let* ([new-seen (set-union seen (list->set last-pts))]
           [ns (flatten (map (lambda (p) (grid-neighbour-points g p))
                             last-pts))]
           [new-pts (filter (lambda (x)
                              (and (not (set-member? new-seen x))
                                   (< (grid-get g x) 9)))
                            ns)])
;      (printf "ns ~a new-pts ~a\n" ns new-pts)
      (if (empty? new-pts)
          new-seen
          (helper new-pts new-seen))))
  (helper (list p) (set)))

(define (basin-size g p)
  (length (set->list (get-basin g p))))

(define basin-sizes (map (lambda (p) (basin-size g p))
                         low-points))

(printf "Basin sizes ~a\n" basin-sizes)

(printf "Mult 3 biggest ~a\n" (apply * (take (sort basin-sizes >) 3)))