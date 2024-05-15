#lang racket

;; Code from FUP lecture on 28nd Feb 2023

(require racket/trace)
(module+ test
  (require rackunit))

;;; Recursive list processing

(define (my-filter val lst)
  (cond
    [(null? lst) '()]
    [(eqv? (car lst) val) (my-filter val (cdr lst))]
    [else
     (cons (car lst)
           (my-filter val (cdr lst)))]))

(module+ test
  (check-equal? (my-filter 'a '(1 a b)) '(1 b)))

(define (my-filter-2 val lst [acc '()])
  (cond
    [(null? lst) (reverse acc)]
    [(equal? (car lst) val) (my-filter-2 val (cdr lst) acc)]
    [else (my-filter-2 val (cdr lst) (cons (car lst) acc))]))

;(trace my-filter-2)

(module+ test
  (check-equal? (my-filter-2 'a '(1 a b)) '(1 b))
  (test-equal? "filter multiple" (my-filter-2 'a '(1 a b a)) '(1 b)))

(define (div-by-3-or-5 v)
  (or (zero? (remainder v 5))
      (zero? (remainder v 3))))

(define (my-filter-3 pred lst [acc '()])
  (cond
    [(null? lst) (reverse acc)]
    [(pred (car lst))
     (my-filter-3 pred (cdr lst) (cons (car lst) acc))]
    [else
     (my-filter-3 pred (cdr lst) acc)]))

(module+ test
  (test-equal? "fizzbuzzy" (my-filter-3 div-by-3-or-5 '(1 2 3 4 5 6)) '(3 5 6)))

(define (bad-maxlist lst)
  (if (null? lst)
      -inf.0
      (if (> (car lst) (bad-maxlist (cdr lst)))
          (car lst)
          (bad-maxlist (cdr lst)))))

;(time (bad-maxlist (range 30)))

(define (better-maxlist lst)
  (if (null? lst)
      -inf.0
      (let ([m (better-maxlist (cdr lst))])
        (if (> (car lst) m) (car lst) m))))

;(time (better-maxlist (range 30)))

(define (best-maxlist lst [acc -inf.0])
  (cond
    [(null? lst) acc]
    [(> (car lst) acc) (best-maxlist (cdr lst) (car lst))]
    [else (best-maxlist (cdr lst) acc)]))

;(time (better-maxlist (range 30)))

;;; Trees
; node of the form: '(data left right)
; accessor functions
(define get-data car)
(define get-left cadr)
(define get-right caddr)

(define btree
  '(1
    (2
     (4
      (7 #f #f)
      #f)
     (5 #f #f))
    (3
     (6
      (8 #f #f)
      (9 #f #f))
     #f)))

(define (find pred tree)
  (if tree
      (let* ([data (get-data tree)]
             [left (find pred (get-left tree))]
             [right (find pred (get-right tree))]
             [both (append left right)])
        (if (pred data)
            (cons data both)
            both))
      '()))
            
;;; Algebraic expressions

; expression ~ number or '(op expression...)
(define the-expression
  '(+ 13
      (* 2 3 4)
      (- 4 2)
      (opp (+ 1 2))))

(define (eval-expr e)
  (if (number? e)
      e
      (let ([op (car e)]
            [children (map eval-expr (cdr e))])
        (cond
          [(eq? op '+) (apply + children)]
          [(eq? op '-) (apply - children)]
          [(eq? op '*) (apply * children)]
          [(eq? op 'opp) (- (car children))]))))

(module+ test
  (test-equal? "eval literal" (eval-expr 13) 13)
  (test-equal? "eval plus" (eval-expr '(+ 1 2 3)) 6)
  (test-equal? "eval times" (eval-expr '(* 1 2)) 2)
  (test-equal? "eval nested" (eval-expr '(+ (opp 1) 1)) 0)
  (test-equal? "eval big expr" (eval-expr the-expression) 36))

(module+ test
  (check-equal? (cons 'a (cons 'c '())) '(a b) "optional message"))

(module+ test
  (test-case "eval-expr tests"
             (check-equal? (eval-expr '(+ 1 2)) 0)
             (check-equal? (eval-expr '(+ (opp 1) 1)) 0)))
