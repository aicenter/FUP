#lang racket

(require racket/trace)
     
; Factorial via linear recursion
(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

; Factorial via linear tail recursion
(define (fact2 n [acc 1])
  (displayln (format "n=~a, acc=~a" n acc))
  (if (<= n 1)
      acc
      (fact2 (- n 1) (* n acc)))) ; tail recursive

(trace fact)
;(trace fact2)

; Tree recursion example
(require graphics/value-turtles)

; the initial empty picture 600x600 and turtle's state
(define init (turtles 600 600 450 500 (* 1.5 pi)))
; constant to modify the fractal's size
(define stick-size 2)

; tree recursive function generating the fractal
(define (tree n [img init])
  (cond [(<= n 1) img]
        [else
         (define stick1 (draw (/ (* stick-size n) 2) img))
         (define state (turtle-state stick1))
         (define left (tree (/ n 2) (turn 60 stick1)))
         (define right (tree (/ n 2) (turn -60 (restore-turtle-state left state))))
         (define stick2 (draw (/ (* stick-size n) 2) 
                              (restore-turtle-state right state)))
         (tree (- n 1) (turn 5 stick2))]))
