#lang racket
(provide grid)

(define (absdiff x y) (abs (- x y)))
(define (manhattan x y) (apply + (map absdiff x y)))

(define/match (nearest-to-char group dist)
    (((list (cons a pt)) 0) a)
    (((list (cons a _)) _) (char-downcase a))
    (((cons _ _) _) #\.))

(define (get-symb pts loc)
  (define (keyfn pt) (manhattan loc (cdr pt)))
  (let ((groups (group-by keyfn (sort pts < #:key keyfn))))
    (nearest-to-char (car groups) (keyfn (caar groups)))))

(define (grid pts)
  (let ((w (apply max (map cadr pts)))
        (h (apply max (map caddr pts))))
    (for/list ((y (add1 h)))
      (for/list ((x (add1 w)))
        (get-symb pts (list x y))))))
