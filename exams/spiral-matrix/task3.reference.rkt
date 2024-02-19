#lang racket

(provide spiral-matrix)

(define (mat-add xss n)
	(map (curry map (curry + n)) xss))

(define (wrap x ys z)
  (cons x (append ys (list z))))

(define (spiral-matrix n)
  (define (extendH x) (map wrap (range (- (* 4 n) 4) (- (* 3 n) 2) -1) x (range (+ n 1) (- (* 2 n) 1))))
  (define (extendV x) (wrap (range 1 (+ 1 n)) x (range (- (* 3 n) 2) (- (* 2 n) 2) -1)))
  (if (equal? 1 n)
    '((1))
    (let ((smaller (spiral-matrix (- n 2))))
	(extendV (extendH (mat-add smaller (- (* 4 n) 4)))))))
