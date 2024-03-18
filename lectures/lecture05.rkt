#lang racket

;;; Lecture 6 - mutable data

; set!
(define x 1)
x
(set! x 2)
x
(set! x '(a b c))
x

(define (make-counter)
  (define cnt 0)
  (lambda ()
    (set! cnt (+ cnt 1))
    cnt))

; pseudorandom number generator
(define random
  (let ([a 69069]
        [b 1]
        [m (expt 2 32)]
        [seed 20210323])
    (lambda args
      (if (null? args)
          (begin
            (set! seed
                  (modulo (+ (* a seed) b) m))
            (exact->inexact (/ seed m)))
          (set! seed (car args))))))

(random)
(random 666)

; vectors
(define v (vector 1 2 3))
v
(vector-ref v 2)
(vector-set! v 2 "hi")
v
(make-vector 4 'a)
(define v2 #('a 'b 'c))  ; v2 is immutable
;(vector-set! v2 1 'd)