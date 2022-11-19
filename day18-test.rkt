#lang racket
(require rackunit "day18.rkt")

(define explode-tcs '(
                      ("[[[[[9,8],1],2],3],4]" "[[[[0,9],2],3],4]")
                      ))

(for ([tc explode-tcs])
  (let* ([s (first tc)]
         [e (second tc)]
         [g (snumberstr-explode s)])
    (check-equal? g e (format "~a" tc))))

#;(define filter-path-tcs
  '(("[[[[[9,8],1],2],3],4]" (#t #t #t #t))))
;    ("[7,[6,[5,[4,[3,2]]]]]" (#f #f #f #f))
;    ("[[6,[5,[4,[3,2]]]],1]" (#f #f #f #t))
;    ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" (#f #f #f #f))
;    ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" (#f #t #t #t))))

#;(for ([tc filter-path-tcs])
  (let* ([sn-str (car tc)]
         [sn (string->snumber sn-str)]
         [match-paths (snumber-filter-path sn explode?)])
    (check-equal? match-paths (cdr tc) sn-str)))