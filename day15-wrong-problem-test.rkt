#lang racket
(require rackunit "day15.rkt")

(define tcs-growing '(
                      (((1 2)
                        (2 3 6))
                       (3 4 8))
                      (((3 4 8)
                        (3 1 8 3))
                       (6 4 12 11))
                      ))
(define tcs-shrinking '(
                        (((1 2 4 3)
                          (3 5 7))
                         (4 7 10))
                        ))

(define (check-tc-g? tc)
  (let* ([a (caar tc)]
         [b (cadar tc)]
         [e (cadr tc)]
         [g (diag-low-sum-growing a b)])
    (printf "e ~v g ~v\n" e g)
    (check-equal? g e)))

(define (check-tc-s? tc)
  (let* ([a (caar tc)]
         [b (cadar tc)]
         [e (cadr tc)]
         [g (diag-low-sum-shrinking a b)])
    (printf "e ~v g ~v\n" e g)
    (check-equal? g e)))

;    (printf "a ~v b ~v e ~v\n" a b e)))

(for ([tc-g tcs-growing])
  (check-tc-g? tc-g))
(for ([tc-s tcs-shrinking])
  (check-tc-s? tc-s))