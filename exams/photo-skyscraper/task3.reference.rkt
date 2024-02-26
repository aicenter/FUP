#lang racket
(provide best-view)

(define (visible-roofs-row row [height 0] [n 0])
  (if (empty? row)
      n
      (if (< height (car row))
          (visible-roofs-row (cdr row) (car row) (+ n 1))
          (visible-roofs-row (cdr row) height n))))

(define (transpose mat) (apply map list mat))


(define (visible-roofs city dir)
  (define m
    (match dir
      ['W city]
      ['E (map reverse city)]
      ['N (transpose city)]
      ['S ((compose (curry map reverse) transpose) city)]))
  (apply + (map visible-roofs-row m)))


(define (best-view city)
  (define views (list (list 'N (visible-roofs city 'N))
                      (list 'S (visible-roofs city 'S))
                      (list 'E (visible-roofs city 'E))
                      (list 'W (visible-roofs city 'W))))
  (define (inner m v) (if (< (cadr m) (cadr v)) v m))
  (define sol (foldl inner (list 'None 0) views))
  (cons (car sol) (cadr sol)))

