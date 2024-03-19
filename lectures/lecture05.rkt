#lang racket

;;; Lecture 5 - Streams and macros
;;; Lazy evaluation

; lazy evaluation of arguments via thunks
(define (my-if c a b)
  (if c a b))

(define (my-lazy-if c a b) (if c (a) (b)))
(my-lazy-if #t (thunk 0) (thunk (/ 1 0)))


;;; Macros

; lazy if
(define-syntax-rule (macro-if c a b)
  (my-lazy-if c (thunk a) (thunk b)))

(macro-if (< 0 1) 'then (/ 1 0))

; list comprehension
(define-syntax list-comp
  (syntax-rules (: <- if) ; these are treated as literals in the pattern matches
    [(list-comp <expr> : <id> <- <lst>)
     (map (lambda (<id>) <expr>) <lst>)]

    [(list-comp <expr> : <id> <- <lst> if <cond>)
     (map (lambda (<id>) <expr>)
          (filter (lambda (<id>) <cond>) <lst>))]))

(list-comp (+ x 2) : x <- (range 10) if (>= 3 x))

; log macro
(define ((log-rt f fsymb) . xs)
  (let ([val (apply f xs)])
    (printf "Function: ~a\n" fsymb)
    (printf "Arguments: ~a\n" xs)
    (printf "Result: ~a\n" val)
    val))

(define-syntax-rule (log f) (log-rt f 'f))

(map (log *) (range 5) (range 5))
(foldl (log cons) '() (range 3))

