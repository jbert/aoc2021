#lang racket
(require "aoc.rkt")

(aoc-set-day 10)
(aoc-set-test #f)

(define lines (aoc-get-lines))

(define char-score
  (hash ")" 3
        "]" 57
        "}" 1197
        ">" 25137))

(define closes-with
  (hash "(" ")"
        "[" "]"
        "{" "}"
        "<" ">"))

(define program (map (lambda (l) (map string (string->list l)))
                     lines))


; Returns:
; - list of needed closing chars (empty if correct)
; - or incorrect string on failure 
(define (parse-progline-helper l stack)
  ;(printf "L ~a S ~a\n" l stack)
  (if (empty? l)
      stack
      (let* ([c (first l)]
             [cw (hash-ref closes-with c "")])
        (if (equal? "" cw)
            ; Not an opener, must match
            (if (equal? c (first stack))
                ; matched
                (parse-progline-helper (rest l) (rest stack))
                ; wrong closer
                c)
            ; an opener
            (parse-progline-helper (rest l) (cons cw stack))))))

(define (parse-progline l)
  (parse-progline-helper l '()))

(define (parse-program p)
  (map parse-progline p))

(define (syntax-error-score p)
  (let* ([illegal-chars (filter string?
                                (parse-program p))]
         [scores (map (lambda (s) (hash-ref char-score s 0))
                      illegal-chars)])
    (apply + scores)))

(printf "syntax error score ~a\n" (syntax-error-score program))

(define (incomplete-closers p)
  (filter (lambda (x) (and (not (string? x))
                           (not (empty? x))))
          (parse-program p)))

;(incomplete-closers program)

(define autocomplete-score-value
  (hash ")" 1
        "]" 2
        "}" 3
        ">" 4))

(define (autocomplete-score l)
  (define (helper l s)
    (if (empty? l)
        s
        (let ([value (hash-ref autocomplete-score-value (first l) #f)])
          (helper (rest l)
                  (+ (* 5 s)
                     value)))))
  (helper l 0))

(define autocomplete-scores (map autocomplete-score
                                (incomplete-closers program)))

(printf "Middle AC score ~a\n"
        (let* ([sorted-scores (sort autocomplete-scores <)]
               [midpos (/ (sub1 (length sorted-scores)) 2)])
          ;(printf "SS ~a\n" sorted-scores)
          ;(printf "midpos ~a\n" midpos)
          (list-nth sorted-scores midpos)))
              