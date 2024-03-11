#lang racket

(require rackunit)
(require plot)
;(require racket/stream)

;;; Lecture 4 - Lazy evaluation

; lazy evaluation of arguments via thunks
(define (my-if c a b)
  (if c a b))

(define (reciprocal x)
  (my-if (= x 0) 0 (/ 1 x)))

(define (my-lazy-if c a b) (if c (a) (b)))
(my-lazy-if #t (thunk 0) (thunk (/ 1 0)))

;;; Streams
(time (stream-fold + 0 (in-range 1000000)))
(time (foldl + 0 (range 1000000)))

(define ps (stream-cons (/ 1 0) (/ 1 0)))
;(stream-first ps)
(stream-rest ps)

;;; Explicit definitions of streams
; naturals
(define (nats n)
  (stream-cons n (nats (+ n 1))))

; Return the first successful application of f on elements of lts
(define (first-success f lst)
  (if (null? lst)                   ; are there any elements to test?
      #f                            ; no - return false
      (let ([res (f (car lst))])    ; yes - apply f to the first element in lst
        (if (eqv? res #f)           ; is the application successul?
            (first-success f (cdr lst)) ; no - try next element
            res                     ; yes - return it
            ))))

; Lazy version
(define (lazy-first-success f st)
  (stream-first (stream-filter identity (stream-map f st))))

(define (test n)
  (if (> n 500) n #f))

(time (car (filter identity (map test (range 0 100000)))))
(time (lazy-first-success test (range 0 100000)))


;;; Explicit definitions
(define (ints-from n)
  (cons n (delay (ints-from (+ n 1)))))

(define naturals (ints-from 0))

(define (repeat f a0)
  (stream-cons a0 (repeat f (f a0))))
 

;;; Implicit definitions
; constant stream
(define ones (stream-cons 1 ones))
(stream->list (stream-take ones 10))

; cyclic streams
(define ab (stream-cons 'a (stream-cons 'b ab)))
(stream->list (stream-take ab 10))

(define abc (stream* 'a 'b 'c abc))
(stream->list (stream-take abc 10))

; stream-append arguments are not delayed
; we can redefine it to define cyclic streams
(define (stream-append s1 s2)
  (if (stream-empty? s1)
      (force s2)
      (stream-cons (stream-first s1) (stream-append (stream-rest s1) s2))))

(define week-days '(mon tue wed thu fri sat sun))
(define stream-days (stream-append week-days (delay stream-days)))

; summing infinite streams
(define (add-streams s1 s2)
  (stream-cons (+ (stream-first s1)
                  (stream-first s2))
               (add-streams (stream-rest s1)
                            (stream-rest s2))))

; natural numbers defined implicitly
(define nats2 (stream-cons 0 (add-streams ones nats2)))
(stream->list (stream-take nats2 10))

;;;  Eager Newtwon-Raphson

(define eps 0.000000000001)
(define (mean . xs) (/ (apply + xs) (length xs)))
(define (next-guess n g) (mean g (/ n g)))
(define (good-enough? n1 n2 eps) (< (abs (- 1 (/ n1 n2))) eps))

; generation & termination are intertwined
(define (my-sqrt n [g 1.0])
  (define new-g (next-guess n g))
  (if (good-enough? g new-g eps)
      new-g
      (my-sqrt n new-g)))

;;; Lazy Newton-Raphson
(define (within eps seq)
  (define fst (stream-first seq))
  (define rest (stream-rest seq))
  (define snd (stream-first rest))
  (if (good-enough? fst snd eps)
      snd
      (within eps rest)))

(define (lazy-sqrt n [g 1.0])
  (within eps (repeat (curry next-guess n) g)))


;;; Depth First Search
(struct arc (source target) #:transparent)
(struct node (data children) #:transparent)

(define g (list (arc 1 2) (arc 2 1)
                (arc 1 5) (arc 2 4)
                (arc 4 5) (arc 5 6)
                (arc 6 3) (arc 3 4)))

(define (get-neighbors g v)
  (map arc-target (filter (lambda (arc) (equal? v (arc-source arc))) g)))

(define (make-tree get-successors v)
  (define successors (get-successors v))
  (node v (stream-map (curry make-tree get-successors) successors)))

(define t (make-tree (curry get-neighbors g) 1))

(define (get-ext-paths g path)
  (define end (car path))
  (define neighbors (get-neighbors g end))
  (map (curryr cons path) neighbors))

(define t-en (make-tree (curry get-ext-paths g) '(1)))

(define (filter-children pred tree)
  (match tree
    [(node data children)
     (node data (stream-map (curry filter-children pred)
                            (filter pred (stream->list children))))]))

(define t-en-f
    (filter-children (compose not check-duplicates node-data) t-en))

(define (dfs goal? tree)
  (match tree
    [(node path children)
     (if (goal? path)
         (reverse path)
         (ormap (curry dfs goal?) (stream->list children)))]))

(dfs (compose (curry eqv? 3) car) t-en-f)
; => '(1 2 4 5 6 3)
