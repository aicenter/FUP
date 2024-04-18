#lang racket
(provide encode)

(define (normalize str)
  (filter char-alphabetic? (string->list (string-downcase str))))

(define (get-num-cols n)
  (exact-ceiling (sqrt n)))

(define (make-rows lst n)
  (let ([len (length lst)])
    (cond
      ([null? lst] '())
      ([< len n] (list (append lst (make-list (- n len) #\space))))
      (else (cons (take lst n) (make-rows (drop lst n) n))))))

(define (transpose lst)
  (apply map list lst))

(define (encode str)
  (let* ([lst (normalize str)]
         [n (get-num-cols (length lst))]
         [rows (transpose (make-rows lst n))])
    (string-join (map list->string rows))))
