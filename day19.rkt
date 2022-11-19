#lang racket
(require "aoc.rkt")
(require "pts.rkt")

(aoc-set-day 19)
(aoc-set-test #t)

(define test-lines (list
                    "--- scanner 0 ---"
                    "0,2"
                    "4,1"
                    "3,3"
                    ""
                    "--- scanner 1 ---"
                    "-1,-1"
                    "-5,0"
                    "-2,1"))

(define the-lines (aoc-get-lines))

(define (parse-lines lines)
  (let ([scan-lines (list-partitionf lines
                                     (lambda (l) (equal? "" l)))])
    ;(printf "~v\n" scan-lines)
    (map parse-scan scan-lines)))

(struct scan (id pts) #:transparent)

(define (parse-scan scan-lines)
  ;(printf "~v\n" scan-lines)
  (let* ([id (string-trim (first scan-lines) "--- scanner ")]
         [id (string-trim id " ---")]
         [pts (map string->p3 (rest scan-lines))])
    (scan id pts)))

(define the-scans (parse-lines the-lines))

(define (scan-delta-set s)
  (list->set (map (lambda (p) (p3-sub (car p) (cdr p)))
                  (half-cartesian-product (scan-pts s)))))

(define (scan-characteristic s)
  (list->set (map p3-sqlen (set->list (scan-delta-set s)))))

(map (lambda (p)
       (let ([s1 (car p)]
             [s2 (cdr p)])
       (printf "~v - ~v: ~v\n"
               (scan-id s1)
               (scan-id s2)
               (length (set->list (set-intersect (scan-characteristic s1)
                                                 (scan-characteristic s2))))))
       1)
     (half-cartesian-product the-scans))