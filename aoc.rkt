#lang racket
(provide
 aoc-get-lines
 aoc-get-stream
 aoc-get-nums
 aoc-set-day
 aoc-set-test

 hash-key-add

 count-inc
 count-inc-foldl
 count-add

 list-nth
 list-partitionf
 
 half-cartesian-product
 )


(define day 0)
(define suffix "")

(define (aoc-set-day newday)
  (set! day newday))

(define (aoc-set-test p)
  (set! suffix (if p "-test" "")))

(define work-dir "/home/john/dev/jbert/aoc2021")

(define (hash-key-add h k v)
  (let ([current (hash-ref h k 0)])
    (hash-set h k (+ current v))))

(define (input-filename)
  (format "~a/data/day~a~a.txt" work-dir day suffix))
(define (aoc-get-lines)
  (file->lines (input-filename)))

(define (aoc-get-stream)
  (open-input-file (input-filename)))

(define (aoc-get-nums)
  (map string->number (aoc-get-lines)))


;;; Add x to the nth position
(define (count-add counts n x)
  (if (= n 0)
      (cons (+ x (first counts))
            (rest counts))
      (cons (first counts)
            (count-add (rest counts) (- n 1) x))))


;;; increment the n'th position of the list
(define (count-inc counts n)
  (count-add counts n 1))

;;; foldl-friendly count-inc
(define (count-inc-foldl n counts)
  (count-inc counts n))


(define (list-nth l n)
  (if (empty? l)
      l
      (if (= n 0)
          (first l)
          (list-nth (rest l) (sub1 n)))))

(define (list-partitionf l pred)
;  (printf "l ~v pred ~v\n" l pred)
  (define-values (f r) (splitf-at l (lambda (x) (not (pred x)))))
;  (printf "f ~v r ~v\n" f r)
  (if (empty? f)
      r
      (if (empty? r)
          (list f)
          (cons f (list-partitionf (rest r) pred)))))

(define (half-cartesian-product l)
  (define (helper a l)
    (map (lambda (b) (cons a b)) l))
  (if (empty? l)
      l
      (append
       (helper (first l) (rest l))
       (half-cartesian-product (rest l)))))