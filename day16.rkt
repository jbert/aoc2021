#lang racket
(require "aoc.rkt")

(aoc-set-day 16)
(aoc-set-test #f)

(define lines (aoc-get-lines))
(define test-cases '(
                     ("8A004A801A8002F478" 16)
                     ("620080001611562C8802118E34" 12)
                     ("C0015000016115A2E0802F182340" 23)
                     ("A0016C880162017C3686B18A3D4780" 31)))

; I know, I know
(define (hexdigit->bits d)
  (case d
    [("0") '(0 0 0 0)]
    [("1") '(0 0 0 1)]
    [("2") '(0 0 1 0)]
    [("3") '(0 0 1 1)]
    [("4") '(0 1 0 0)]
    [("5") '(0 1 0 1)]
    [("6") '(0 1 1 0)]
    [("7") '(0 1 1 1)]
    [("8") '(1 0 0 0)]
    [("9") '(1 0 0 1)]
    [("A") '(1 0 1 0)]
    [("B") '(1 0 1 1)]
    [("C") '(1 1 0 0)]
    [("D") '(1 1 0 1)]
    [("E") '(1 1 1 0)]
    [("F") '(1 1 1 1)]
    [else (error (format "Unrecognised digit ~v\n" d))]))
    
(define (hex->bs s)
  (apply append (map hexdigit->bits (map string (string->list s)))))
(define (bs->number bs)
  (define (helper bs v)
    (if (empty? bs)
        v
        (if (= (first bs) 0)
            (helper (rest bs) (* 2 v))
            (helper (rest bs) (+ 1 (* 2 v))))))
  (helper bs 0))


(struct packet (version type contents) #:transparent)

(struct parse-step (bs length value) #:transparent)

(define (parse-header bs)
  (let* ([version (bs->number (take bs 3))]
         [bs (drop bs 3)]
         [type (bs->number (take bs 3))]
         [bs (drop bs 3)])
    (parse-step bs (+ 3 3) (list version type))))

(define (parse-literal bs)
  (define (helper bs)
    (let* ([bits (take bs 5)]
           [bs (drop bs 5)])
      (if (= (first bits) 0)
          (parse-step bs 5 (rest bits))
          (let ([ps (helper bs)])
            (parse-step (parse-step-bs ps)
                        (+ 5 (parse-step-length ps))
                        (append (rest bits) (parse-step-value ps)))))))
  (let* ([ps (helper bs)]
         [v (bs->number (parse-step-value ps))])
;    (printf "bits ~v\n" (parse-step-value ps))
    (struct-copy parse-step ps [value v])))

(define (parse-packet bs)
  (let* ([psh (parse-header bs)]
         [version (first (parse-step-value psh))]
         [type (second (parse-step-value psh))]
         [bs (parse-step-bs psh)])
;    (printf "Version ~v type ~v\n" version type)
    (if (= type 4)
        (let* ([psl (parse-literal bs)])
          (parse-step (parse-step-bs psl)
                      (+ (parse-step-length psh)
                         (parse-step-length psl))
                      (packet version type (parse-step-value psl))))
        (let* ([lti (bs->number (take bs 1))]
               [bs (drop bs 1)])
          (if (= lti 0)
              (let* ([bit-length (bs->number (take bs 15))]
                     [bs (drop bs 15)]
                     [psp (parse-packets-bit-length bs bit-length)])
                (parse-step (parse-step-bs psp)
                            (+ 1
                               15
                               (parse-step-length psh)
                               (parse-step-length psp))
                            (packet version type (parse-step-value psp))))
              (let* ([packet-count (bs->number (take bs 11))]
                     [bs (drop bs 11)]
                     [psp (parse-packets-count bs packet-count)])
                (parse-step (parse-step-bs psp)
                            (+ 1
                               11
                               (parse-step-length psh)
                               (parse-step-length psp))
                            (packet version type (parse-step-value psp)))))))))

(define (parse-packets-count bs n)
  (if (= n 0)
      (parse-step bs 0 '())
      (let* ([this-step (parse-packet bs)]
             [bs (parse-step-bs this-step)]
             [other-steps (parse-packets-count bs (sub1 n))])
        (parse-step (parse-step-bs other-steps)
                    (+ (parse-step-length this-step)
                       (parse-step-length other-steps))
                    (cons (parse-step-value this-step)
                          (parse-step-value other-steps))))))

(define (parse-packets-bit-length bs n)
  (if (= n 0)
      (parse-step bs 0 '())
      (let* ([this-step (parse-packet bs)]
             [bs (parse-step-bs this-step)]
             [other-steps (parse-packets-bit-length bs
                                                    (- n (parse-step-length this-step)))])
        (parse-step (parse-step-bs other-steps)
                    (+ (parse-step-length this-step)
                       (parse-step-length other-steps))
                    (cons (parse-step-value this-step)
                          (parse-step-value other-steps))))))
              



;(define tt "110100101111111000101000")
;(define tt "11101110000000001101010000001100100000100011000001100000")
;(define tt "00111000000000000110111101000101001010010001001000000000")
;(define ttbs (map string->number (map string (string->list tt))))
;(define tt "8A004A801A8002F478")
;(define ttbs (hex->bs tt))
;(parse-packet ttbs)

(define (version-sum pkt)
  (+ (packet-version pkt)
     (if (= (packet-type pkt) 4)
         0
         (apply + (map version-sum (packet-contents pkt))))))

(for ([tc test-cases])
  (let* ([s (first tc)]
         [e (second tc)]
         [bs (hex->bs s)]
         [pkt (parse-step-value (parse-packet bs))])
    (printf "~v -> ~v\n" s e)
    (printf "~v\n" pkt)
    (printf "Version sum: ~v\n" (version-sum pkt))))


(let* ([hexstr (first lines)]
       [bs (hex->bs hexstr)]
       [pkt (parse-step-value (parse-packet bs))])
  (printf "Part 1: version sum ~v\n" (version-sum pkt)))

(define (packet-evaluate pkt)
  (let ([t (packet-type pkt)])
    (if (= t 4)
        (packet-contents pkt)
        (let ([vs (map packet-evaluate (packet-contents pkt))])
          (cond
            [(= t 0) (apply + vs)]
            [(= t 1) (apply * vs)]
            [(= t 2) (apply min vs)]
            [(= t 3) (apply max vs)]
            [(= t 5) (if (apply > vs) 1 0)]
            [(= t 6) (if (apply < vs) 1 0)]
            [(= t 7) (if (apply = vs) 1 0)]
            [else (error (format "Unknown type ~v" t))])))))

(define (bs->packet bs)
  (let ([ps (parse-packet bs)])
    (parse-step-value ps)))

(packet-evaluate (bs->packet (hex->bs "CE00C43D881120")))




(let* ([hexstr (first lines)]
       [bs (hex->bs hexstr)]
       [pkt (parse-step-value (parse-packet bs))])
  (printf "Part 2: packet value ~v\n" (packet-evaluate pkt)))
