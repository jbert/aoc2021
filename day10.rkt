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


; Returns the open stack at end of line
; or string on failure ("" for incomplete, incorrect closes for corrupt)
(define (parse-progline-helper l stack)
  ;(printf "L ~a S ~a\n" l stack)
  (if (empty? l)
      (if (empty? stack)
          '()
          "")
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
  (let* ([illegal-chars (filter (lambda (s) (not (equal? "" s)))
                                (parse-program p))]
         [scores (map (lambda (s) (hash-ref char-score s 0))
                      illegal-chars)])
    (apply + scores)))

(printf "syntax error score ~a\n" (syntax-error-score program))                
  