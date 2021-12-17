#lang racket
(provide graph-make
         graph-edges
         graph-points

         graph-edges-from
         graph-edges-to
         graph-adjacent

         graph-remove-point
         graph-remove-edge

         graph-size
         graph-has-edge?
         graph-has-point?
         )

(struct graph (points edges) #:transparent)

(define (graph-make lines)
  (define (line->edge l)
    (let ([bits (map string->symbol (string-split l "-"))])
      (cons (first bits)
            (second bits))))
  (let* ([edges (map line->edge lines)]
         [points (edges->points edges)]
         [reversed-edges (map reversed-edge edges)])
    (graph points (set-union edges reversed-edges))))

(define (edges->points edges)
  (let* ([from-pts (map car edges)]
         [to-pts (map cdr edges)]
         [points (set-union (list->set from-pts)
                            (list->set to-pts))])
    points))

(define (graph-edges-from g p)
  (filter (lambda (e) (equal? (car e) p))
          (set->list (graph-edges g))))

(define (graph-edges-to g p)
  (filter (lambda (e) (equal? (cdr e) p))
          (set->list (graph-edges g))))

(define (graph-remove-point g p)
  (let* ([new-pts (set-remove (graph-points g) p)]
         [from-edges (graph-edges-from g p)]
         [to-edges (graph-edges-to g p)]
         [new-edges (set-subtract (graph-edges g)
                                  to-edges
                                  from-edges)])
    (graph new-pts new-edges)))

(define (graph-adjacent g p)
  (let* ([edges-from (graph-edges-from g p)])
    (map cdr edges-from)))

(define (graph-remove-edge g e)
  (let* ([new-edges (set-subtract (graph-edges g)
                                  (list e (reversed-edge e)))]
         [new-points (edges->points new-edges)])
    (graph new-points new-edges)))

(define (reversed-edge e)
  (cons (cdr e) (car e)))

(define (graph-has-edge? g e)
  (set-member? (graph-edges g) e))

(define (graph-has-point? g p)
  (set-member? (graph-points g) p))


(define (graph-size g)
  (length (set->list (graph-points g))))