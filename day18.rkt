#lang racket
(require "aoc.rkt")
(aoc-set-day 18)
(aoc-set-test #t)

(provide string->snumber
         snumberstr-explode
         )

(define lines (aoc-get-lines))

(define (string->snumber s)
  (let* ([s (string-replace s
                            ","
                            " . "
                            #:all? #t)]
         [input-port (open-input-string s)])
    (read input-port)))

(define (snumber->string sn)
  (if (number? sn)
      (number->string sn)
      (format "[~a,~a]"
              (snumber->string (first sn))
              (snumber->string (second sn)))))

(define (lines->snumbers lines)
  (map string->snumber lines))

(define (snumber-add a b)
  (list a b))

(define (snumber-split a)
  (let ([adiv2 (/ a 2)])
    (list (floor adiv2) (ceiling adiv2))))

#;(define (snumber-filter-path sn pred)
  (define (helper sn pred rev-path result-paths)
    (let* ([this-node? (pred sn (add1 (length rev-path)))]
           [result-paths (if this-node?
                             (cons (reverse rev-path) result-paths)
                             result-paths)])
      (if (number? sn)
          result-paths
          (let ([lpath (helper (car sn) pred (cons #t rev-path) result-paths)]
                [rpath (helper (cdr sn) pred (cons #f rev-path) result-paths)])
            (printf "LP ~v P ~v RP ~v\n" lpath result-paths rpath)
            (append lpath result-paths rpath)))))
  (helper sn pred '() '()))

(define (delta-depth s)
  ; "[" each prefix is down, each "]" is up
  (let* ([l (string-split s "")]
         [d (count (lambda (c) (equal? c "[")) l)]
         [u (- (count (lambda (c) (equal? c "]")) l))])
    (+ d u)))

(define (parse-cstr cstr)
  (let* ([cstr (string-trim cstr "[" #:repeat? #t)]
         [cstr (string-trim cstr "]" #:repeat? #t)])
    (string->number cstr)))

(define (snumberstr-explode sns)
  (define (helper l depth re)
    (if (empty? l)
        '()
        (let* ([cstr (first l)]
               [depth (+ depth (delta-depth cstr))]
               [c (parse-cstr cstr)]
               [prefix "
               )
          (printf "cstr ~v\n" cstr)
          (printf "l ~v\n" l)
          (cond
            [(equal? re #t)
             (cons 0 (helper (rest l) depth c))]
            [(and (= re 0) (= depth 4))
             (cons 0 (helper (rest l) depth #t))]
            [(not (= re 0))
             (cons (+ c re) (rest l))]
            [(= re 0)
             (cons
            [else (error "whoops")]
            ))))
  (let ([l (string-split sns ",")])
    (let* ([new-l (helper l 0 0)])
      (printf "new-l ~v\n" new-l))))

(snumberstr-explode "[[[[[9,8],1],2],3],4]")