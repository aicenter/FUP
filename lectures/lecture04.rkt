#lang racket

(require rackunit)
;(require racket/stream)

;;; Lecture 4 - Lazy evaluation

; lazy evaluation of arguments via thunks
(define (my-if c a b)
  (if c a b))

(define (reciprocal x)
  (my-if (= x 0) 0 (/ 1 x)))

(define (my-lazy-if c a b) (if c (a) (b)))
(my-lazy-if #t (thunk 0) (thunk (/ 1 0)))

; pass a thunk executed in a new thread
(thread
 (thunk
  (let ([x (foldl + 0 (range 0 10000000))])
    (displayln x))))

;;; Streams
; My stream application
(define (ints-from n)
  (cons n (delay (ints-from (+ n 1)))))

(define nats (ints-from 0))
(force (cdr nats))

; Racket stream implementation
(define (stream-from n)
  (stream-cons n (stream-from (+ n 1))))

(define stream-nats (stream-from 0))
(stream->list (stream-take stream-nats 10))

(define (log x)
  (printf "Logging: ~a~n" x)
  x)

(define st (stream (log 1) (log 2) (log 3)))
(stream-first st)
(stream-rest st)
(stream-first (stream-rest st))

; Return the first successful application of f on elements of lts
(define (first-success f lst)
  (if (null? lst)                   ; are there any elements to test?
      #f                            ; no - return false
      (let ([res (f (car lst))])    ; yes - apply f to the first element in lst
        (if (eqv? res #f)           ; is the application successul?
            (first-success f (cdr lst)) ; no - try next element
            res                     ; yes - return it
            )
        )
      )
  )

; Lazy version
(define (lazy-first-success f st)
  (stream-first (stream-filter identity (stream-map f st)))
  )

(define (test n)
  ;(displayln n)
  (if (> n 500) n #f)
  )

(time (car (filter identity (map test (range 0 100000)))))
(time (lazy-first-success test (range 0 100000)))
                          
;; Macros

; lazy if
(define-syntax macro-if
  (syntax-rules ()
    ((macro-if c a b)
       (my-lazy-if c (thunk a) (thunk b)))))

(macro-if (null? '()) '() (car '()))

; list comprehension
(define-syntax list-comp
  (syntax-rules (: <- if)
    [(list-comp <expr> : <id> <- <lst>)
     (map (lambda (<id>) <expr>) <lst>)]

    [(list-comp <expr> : <id> <- <lst> if <cond>)
     (map (lambda (<id>) <expr>)
          (filter (lambda (<id>) <cond>) <lst>))]
    )
  )

(list-comp (+ x 2) : x <- '(2 3 5) if (>= 3 x))
