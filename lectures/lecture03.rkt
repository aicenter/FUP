#lang racket
(require graphics/value-turtles)

;;; Lecture 3 - higher-order functions

;;; Higher-order functions for list
; map applications

(map (lambda (x) (* 2 x)) '(1 2 3))

; sum of vectors
(map + '(1 2 3) '(3 2 1) '(-4 -4 -4))

; sum of vectors in a list
(apply map + '((1 2 3)
               (3 2 1)
               (-4 -4 -4)))

; get diagonal of a matrix
(map list-ref '((1 2 3)
                (4 5 6)
                (7 8 9)) (range 0 3))

; transpose of a matrix
(apply map list '((1 2 3)
                  (4 5 6)
                  (7 8 9)))

; folding
(define (trace mat)
  (foldl + 0 (map list-ref mat (range 0 (length mat)))))

(trace '((1 2 3) (4 5 6) (7 8 9)))

(foldl cons '() '(a b c))
(foldr cons '(d f g) '(a b c))
(foldl min +inf.0 '(0 -3 2))

(define (move cmd pos)
  (cond
    [(eq? cmd 'up) (map + pos '(0 1))]
    [(eq? cmd 'down) (map + pos '(0 -1))]
    [(eq? cmd 'left) (map + pos '(-1 0))]
    [(eq? cmd 'right) (map + pos '(1 0))]))

(foldl move '(0 0) '(up right right up left))

;;; Currying

(list-ref '(a b c) 2)

; curryfication of list-ref manually
(define (curried-list-ref lst) 
  (lambda (i) (list-ref lst i)))

((curried-list-ref '(a b c)) 2) 

; automatic curryfication by the curry function
(((curry list-ref) '(a b c)) 2)

; simplified syntax
(define ((curried-list-ref2 lst) i)
  (list-ref lst i))

((curry list-ref '(a b c)) 2)
(map (curry * 2) '(1 2 3))

; another example on currying
(define ((log f) . xs)
  (let ([val (apply f xs)])
    (printf "Arguments: ~a\n" xs)
    (printf "Result: ~a\n" val)
    val))

(map (log (curry * 2)) '(1 2 3))

;;; Compose
; traditional style specifying the input str
(define (str->alpha str)
  (list->string (filter char-alphabetic? (string->list str))))

; point-free style (no name for input)
(define str->alpha2
  (compose list->string
           (curry filter char-alphabetic?)
           string->list))

;;; Example - morphic sequences
(define ((phi im0 im1) x)
  (cond
    [(= x 0) im0] 
    [(= x 1) im1])) 

(define (apply-morphism morphism w)
  (apply append (map morphism w)))

(define (generate-seq morphism w0 k)
  (foldl apply-morphism w0 (make-list k morphism)))

;(define ((alpha ang0 ang1) x)
;  (cond
;    [(= x 0) ang0]    
;    [(= x 1) ang1]))   
(define alpha (phi '(0 1 1) '(0)))

(define morphic-seq (generate-seq (phi '(0 1 1) '(0)) '(0) 15))
(define angle-seq (map (phi (/ (* 7 pi) 9) (/ (* -2 pi) 9)) morphic-seq))

(define morphic-seq2 (generate-seq (phi '(0 0) '(1 0 1)) '(1) 10))
(define angle-seq2 (map (phi (/ (* 5 pi) 16) (/ (* -29 pi) 60)) morphic-seq2))

(define init (turtles 800 800))

(define ((turn-draw length) angle img)
  (draw length (turn/radians angle img)))

(define ex1 (foldl (turn-draw 12) init angle-seq))
(define ex2 (foldl (turn-draw 20) init angle-seq2))

;;; Saving images
(require pict)

(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))

;(save-pict (turtles-pict ex1) "ex1.png" 'png)
;(save-pict (turtles-pict ex2) "ex2.png" 'png)

; Pairs via closures
(define (point x y)
  (lambda (m) (m x y)))

(define (get-x p)
  (p (lambda (x y) x)))

(define (get-y p)
  (p (lambda (x y) y)))

(define p (point 3 10))
(get-x p)
(get-y p)

;;; Structures
(struct person (first-name surname age)
  #:transparent)

(define pers (person "John"
                     "Down"
                     33))

(person-first-name pers)
(person-surname pers)
(person-age pers)
(person? pers)
(person? "John")
