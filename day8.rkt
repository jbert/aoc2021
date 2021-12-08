#lang racket
(require "aoc.rkt")

(aoc-set-day 8)
(aoc-set-test #f)

(define lines (aoc-get-lines))

(define (parse-line l)
  (define (parse-seqs s)
    (map (lambda (char-seq)
           (map (lambda (c) (string->symbol (string c)))
                (sort char-seq char<?)))
         (map string->list (string-split s " "))))
  (let* ([bits (string-split l " | ")])
    (list (parse-seqs (first bits))
          (parse-seqs (second bits)))))
(define entries (map parse-line lines))

(define (is-1478? seq)
  (let ([l (length seq)])
    (or (= l 2) ; 1
        (= l 4) ; 4
        (= l 3) ; 7
        (= l 7)))) ; 8

(printf "Number of 1478 output seqs: ~a\n"
        (let ([outputs (apply append (map second entries))])
          (length (filter is-1478? outputs))))


;(define (length->numbers l)
;  (case l
;    [(2) '(1)]
;    [(3) '(7)]
;    [(4) '(4)]
;    [(5) '(2 3 5)]
;    [(6) '(0 6 9)]
;    [(7) '(8)]
;    [else (error (format "Impossible length: ~a" l))]))

;(define (number->positions n)
;  (case n
;    [(0) '(a b c e f g)]
;    [(1) '(c f)]
;    [(2) '(a c d e g)]
;    [(3) '(a c d f g)]
;    [(4) '(b c d f)]
;    [(5) '(a b d f g)]
;    [(6) '(a b d e f g)]
;    [(7) '(a c f)]
;    [(8) '(a b c d e f g)]
;    [(9) '(a b c d f g)]
;    [else (error (format "Bad number: ~a" n))]))

(define (count-append l x n)
  (if (= n 0)
      (cons (cons x (first l))
            (rest l))
      (cons (first l)
            (count-append (rest l) x (sub1 n)))))


(define (patterns->length-group ps)
  (define (plg-helper ps lg)
    (if (empty? ps)
        lg
        (plg-helper (rest ps)
                    (count-append lg
                                  (list->set (first ps))
                                  (length (first ps))))))
  (plg-helper ps (make-list 8 '())))


(define (lg-get lg l)
  (if (= l 0)
      (first lg)
      (lg-get (rest lg) (sub1 l))))

(define (solve-a lg)
  (let* ([two (first (lg-get lg 2))]
         [three (first (lg-get lg 3))]
         [diff (set-subtract three two)])
    (when (not (= (length (set->list diff))
                  1))
      (error (format "solve-a found diff ~a" diff)))
    diff))

(define (solve-cf lg a)
  (let ([three (first (lg-get lg 3))])
    (set-subtract three a)))

(define (solve-bd lg cf)
  (let ([four (first (lg-get lg 4))])
    (set-subtract four cf)))

(define (solve-adg lg)
  (let ([fives (lg-get lg 5)])
    (when (not (= (length fives) 3))
      (error "don't have 3 five-lengths"))
    (apply set-intersect fives)))
                   
(define (solve-f lg a b d g)
  (let* ([fives (lg-get lg 5)]
         [abdg (set-union a b d g)]
         [digit-five
          (filter (lambda (digit)
                    (= 4 (length (set->list (set-intersect digit abdg)))))
                  fives)])
    (when (not (= (length digit-five)
                  1))
      (error (format "Bad filter in solve f: ~a" digit-five)))
    (set-subtract (first digit-five) abdg)))

(define (solve-length-group lg)
; (printf "LG ~a\n" lg)
  (let* ([a (solve-a lg)]
         [cf (solve-cf lg a)]
         [bd (solve-bd lg cf)]
         [adg (solve-adg lg)]
         [dg (set-subtract adg a)]
         [d (set-intersect dg bd)]
         [b (set-subtract bd d)]
         [g (set-subtract dg d)]
         [f (solve-f lg a b d g)]
         [c (set-subtract cf f)]
         [e (set-subtract (set 'a 'b 'c 'd 'e 'f 'g)
                          (set-union a b c d f g))])
    (define (s x) (first (set->list x)))
;    (printf "a ~a cf ~a\n" a cf)
;    (printf "soln: ~a\n" (list a b c d e f g))
    (hash (s a) 'a
          (s b) 'b
          (s c) 'c
          (s d) 'd
          (s e) 'e
          (s f) 'f
          (s g) 'g)))

(define (unscramble-pattern pattern soln)
;  (printf "P ~a S ~a\n" pattern soln)
  (sort (map (lambda (s) (hash-ref soln s))
             pattern)
        symbol<?))

(define (pattern->digit pattern)
  (case pattern
    ['(a b c e f g) "0"]
    ['(c f) "1"]
    ['(a c d e g) "2"]
    ['(a c d f g) "3"]
    ['(b c d f) "4"]
    ['(a b d f g) "5"]
    ['(a b d e f g) "6"]
    ['(a c f) "7"]
    ['(a b c d e f g) "8"]
    ['(a b c d f g) "9"]
    [else (error (format "bad pattern ~a" pattern))]))

(define (output-value entry)
  (let* ([patterns (first entry)]
         [scrambled-outputs (second entry)]
         [soln (solve-length-group (patterns->length-group patterns))]
         [unscrambled-outputs (map (lambda (pattern)
                                     (unscramble-pattern pattern soln))
                                   scrambled-outputs)]
         [output-digits (map pattern->digit unscrambled-outputs)])
    (string->number (apply string-append output-digits))))

(define lg (patterns->length-group (first (first entries))))

;(define eg (patterns->length-group (first (first entries))))
(printf "Part2: ~a\n" (apply + (map output-value entries)))
